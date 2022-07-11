
#ifndef SIJSON_INTERNAL_BUFFERS_HPP
#define SIJSON_INTERNAL_BUFFERS_HPP

#include <cstddef>
#include <limits>
#include <memory>
#include <type_traits>
#include <ios>
#include <streambuf>
#include <string>
#include <algorithm>
#include <utility>
#include <stdexcept>

#include "common.hpp"
#include "util.hpp"


namespace json {
namespace internal {

// Dynamically-allocated resizable buffer.
//
// Elements are value-initialized only if necessary (eg. if alloc.construct() is non-trivial).
// All member functions provide a strong exception guarantee if alloc.destroy() does not throw.
// 
// https://en.cppreference.com/w/cpp/named_req/Allocator
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p1072r10.html
// 
template <typename T, typename Allocator = std::allocator<T>>
class buffer : iutil::alloc_aware_container<Allocator>
{
private:
    using base = iutil::alloc_aware_container<Allocator>;
    using Altraits = std::allocator_traits<Allocator>;

public:
    buffer(std::size_t init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        base(alloc), 
        m_size(iutil::recommend_alloc_size<T>(this->alloc(), init_capacity)), 
        m_bufp(iutil::alloc_new<T>(this->alloc(), m_size))
    {}

    buffer(const Allocator& alloc = Allocator()) :
        buffer(16, alloc)
    {}

    buffer(buffer&& rhs) :
        base(rhs), m_size(rhs.m_size),
        m_bufp(rhs.m_bufp)
    {
        rhs.m_bufp = nullptr;
    }

    buffer(const buffer& rhs) :
        base(rhs), m_size(rhs.m_size),
        m_bufp(iutil::alloc_new<T>(this->alloc(), rhs.m_size))
    {
        iutil::exception_guard<T, Allocator> guard(this->alloc(), m_bufp, m_size);
        std::copy(rhs.m_bufp, rhs.m_bufp + rhs.m_size, m_bufp);
        guard.disable();
    }

    buffer& operator=(const buffer& rhs)
    {
        if (this != &rhs)
        {
            if (Altraits::propagate_on_container_copy_assignment::value)
            {
                if (iutil::alloc_is_always_equal<Allocator>::value ||
                    this->alloc() == rhs.alloc())
                {
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                    copy_assign(rhs, this->alloc());
                }
                else {
                    // else always reallocate (see Allocator requirements)
                    auto alloc = rhs.alloc();
                    copy_assign<true>(rhs, alloc);
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                }               
            }
            else copy_assign(rhs, this->alloc());
        }
        return *this;
    }

    buffer& operator=(buffer&& rhs)
    {
        if (this != &rhs)
        {
            if (iutil::alloc_always_equal_after_move<Allocator>() ||
                this->alloc() == rhs.alloc())
            {                
                iutil::alloc_unchecked_delete(this->alloc(), m_bufp, m_size);
                // steal
                m_bufp = rhs.m_bufp;
                rhs.m_bufp = nullptr;
                m_size = rhs.m_size;
            }
            // else copy (see Allocator requirements)
            else copy_assign(rhs, this->alloc());

            iutil::move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        }
        return *this;
    }

    inline std::size_t capacity(void) const noexcept { return m_size; }

    inline void reserve(std::size_t rqd_cap)
    {
        if (rqd_cap <= capacity())
            return;

        grow_and_copy(this->alloc(), rqd_cap, m_bufp, m_size);
    }    

    inline T* begin(void) noexcept { return m_bufp; }
    inline T* end(void) noexcept { return m_bufp + m_size; }

    inline const T* begin(void) const noexcept { return m_bufp; }
    inline const T* end(void) const noexcept { return m_bufp + m_size; }
   
    inline T& operator[](std::size_t index) noexcept { return m_bufp[index]; }
    inline const T& operator[](std::size_t index) const noexcept { return m_bufp[index]; }

    inline memspan<char> data(void) noexcept { return { begin(), end() }; }
    inline memspan<const char> data(void) const noexcept { return { begin(), end() }; }

    ~buffer(void)
    {
        iutil::alloc_delete(this->alloc(), m_bufp, m_size);
    }

private:
    // Assumes this != &rhs.
    template <bool must_reallocate = false>
    inline void copy_assign(const buffer& rhs, Allocator& alloc)
    {
        if (must_reallocate || m_size != rhs.m_size)
        {
            reallocate_and_copy(alloc, rhs.m_size, rhs.m_bufp, rhs.m_size);
        } 
        else std::copy(rhs.m_bufp, rhs.m_bufp + rhs.m_size, m_bufp);
    }

    inline void reallocate_and_copy(Allocator& alloc, std::size_t new_cap, const T* src, std::size_t len)
    {
        assert(new_cap >= len && new_cap != m_size);

        T* bufp = iutil::alloc_new<T>(alloc, new_cap);
        iutil::exception_guard<T, Allocator> guard(alloc, bufp, new_cap);

        std::copy(src, src + len, bufp);
        iutil::alloc_unchecked_delete(this->alloc(), m_bufp, m_size);

        m_bufp = bufp;
        m_size = new_cap;

        guard.disable();
    }

    inline void grow_and_copy(Allocator& alloc, std::size_t rqd_cap, const T* src, std::size_t len)
    {
        auto new_cap = iutil::recommend_vector_growth<T>(alloc, m_size, rqd_cap);
        reallocate_and_copy(alloc, new_cap, src, len);
    }

private:
    std::size_t m_size;
    T* m_bufp;
};



// String buffer with short-string optimization.
//
// Elements are value-initialized only if necessary (eg. if alloc.construct() is non-trivial).
// All member functions provide a strong exception guarantee if alloc.destroy() does not throw.
//
template <typename T, 
    typename Traits = std::char_traits<T>, 
    typename Allocator = std::allocator<T>
>
class strbuffer : iutil::alloc_aware_container<Allocator>
{
private:
    using base = iutil::alloc_aware_container<Allocator>;
    using Altraits = std::allocator_traits<Allocator>;

    struct long_data
    {
        unsigned char is_long : 1;
        std::size_t cap;
        std::size_t len;
        T* bufp;
    };

    static constexpr std::size_t SHORT_STRING_CAP =
        (sizeof(long_data) - 1) / sizeof(T) > 1 ?
        (sizeof(long_data) - 1) / sizeof(T) : 1;

    struct short_data
    {
        unsigned char is_long : 1;
        unsigned char len : 7;
        T buf[SHORT_STRING_CAP];
    };

public:
    static_assert(iutil::is_pod<T>::value, "T must be a plain-old-data type.");

    // Guaranteed to be at least 1.
    static constexpr std::size_t min_capacity = SHORT_STRING_CAP;

public:
    // Initial capacity should include null-terminator (if any).
    strbuffer(std::size_t init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        base(alloc)
    {
        bool sso = init_capacity <= SHORT_STRING_CAP;
        is_long(!sso); // activate

        if (sso) short_len(0);
        else
        {
            auto new_cap = iutil::recommend_alloc_size<T>(this->alloc(), init_capacity);
            long_bufp(iutil::alloc_new<T>(this->alloc(), new_cap));
            long_cap(new_cap);
            long_len(0);
        }
    }

    strbuffer(const Allocator& alloc = Allocator()) :
        strbuffer(SHORT_STRING_CAP, alloc)
    {}

    strbuffer(strbuffer&& rhs) : 
        base(rhs), m_data(rhs.m_data)
    {
        if (rhs.is_long())
            rhs.long_bufp(nullptr);
    }

    strbuffer(const strbuffer& rhs) : 
        base(rhs)
    {
        if (!rhs.is_long())
            m_data = rhs.m_data;
        else
        {
            this->is_long(false); // default-init
            copy_assign(rhs, this->alloc());
        }
    }

    strbuffer& operator=(const strbuffer& rhs)
    {
        if (this != &rhs)
        {
            if (Altraits::propagate_on_container_copy_assignment::value)
            {
                if (iutil::alloc_is_always_equal<Allocator>::value ||
                    this->alloc() == rhs.alloc())
                {
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                    copy_assign(rhs, this->alloc());
                }
                else {
                    // else always reallocate (see Allocator requirements)
                    auto alloc = rhs.alloc();
                    copy_assign<true>(rhs, alloc);
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                }
            }
            else copy_assign(rhs, this->alloc());
        }
        return *this;
    }

    strbuffer& operator=(strbuffer&& rhs)
        noexcept(iutil::alloc_always_equal_after_move<Allocator>())
    {
        if (this != &rhs)
        {
            if (iutil::alloc_always_equal_after_move<Allocator>() ||
                this->alloc() == rhs.alloc())
            {
                if (this->is_long())
                    iutil::alloc_unchecked_delete(this->alloc(), long_bufp(), long_len());

                // steal
                m_data = rhs.m_data;
                if (rhs.is_long()) 
                    rhs.long_bufp(nullptr);
            }
            // else copy (see Allocator requirements)
            else copy_assign(rhs, this->alloc());

            iutil::move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        }
        return *this;
    }

    inline std::size_t length(void) const noexcept { return is_long() ? long_len() : short_len(); }
    inline std::size_t capacity(void) const noexcept { return is_long() ? long_cap() : SHORT_STRING_CAP; }

    inline void reserve(std::size_t rqd_cap)
    {
        if (rqd_cap <= capacity())
            return;

        grow(rqd_cap);
    }

    // Mark available capacity as under use.
    inline void commit(std::size_t count)
    {
        assert(length() + count <= capacity());
        is_long() ? long_len(long_len() + count) : short_len(short_len() + count);
    }

    inline T* begin(void) noexcept 
    { return is_long() ? long_bufp() : short_bufp(); }

    inline T* end(void) noexcept 
    { return is_long() ? long_bufp() + long_len() : short_bufp() + short_len(); }

    inline const T* begin(void) const noexcept 
    { return is_long() ? long_bufp() : short_bufp(); }

    inline const T* end(void) const noexcept 
    { return is_long() ? long_bufp() + long_len() : short_bufp() + short_len(); }

    inline T& operator[](std::size_t index) noexcept { return begin()[index]; }
    inline const T& operator[](std::size_t index) const noexcept { return begin()[index]; }

    inline memspan<char> data(void) noexcept { return { begin(), end() }; }
    inline memspan<const char> data(void) const noexcept { return { begin(), end() }; }

    ~strbuffer(void)
    {
        if (is_long())
            iutil::alloc_delete(this->alloc(), long_bufp(), long_cap());
    }

private:
    template <bool this_is_short>
    inline void grow_and_copy(Allocator& alloc, std::size_t rqd_cap, const T* src, std::size_t len)
    {
        assert(rqd_cap >= len && rqd_cap > capacity());

        auto new_cap = iutil::recommend_vector_growth<T>(alloc, capacity(), rqd_cap);

        T* bufp = iutil::alloc_new<T>(alloc, new_cap);
        iutil::exception_guard<T, Allocator> guard(alloc, bufp, new_cap);

        Traits::copy(bufp, src, len);

        if (!this_is_short)
            iutil::alloc_unchecked_delete(this->alloc(), long_bufp(), long_cap());
        else this->is_long(true);

        long_bufp(bufp);
        long_cap(new_cap);
        long_len(len);

        guard.disable();
    }

    // Assumes this != &rhs.
    template <bool this_is_short>
    inline void grow_and_copy(Allocator& alloc, const strbuffer& rhs)
    {
        grow_and_copy<this_is_short>(alloc, rhs.long_len(), rhs.long_bufp(), rhs.long_len());
    }

    inline void grow(std::size_t rqd_cap)
    {
        if (!this->is_long())
            grow_and_copy<true>(this->alloc(), rqd_cap, short_bufp(), short_len());
        else
            grow_and_copy<false>(this->alloc(), rqd_cap, long_bufp(), long_len());
    }

    // Assumes this != &rhs.
    template <bool must_reallocate = false>
    inline void copy_assign(const strbuffer& rhs, Allocator& alloc)
    {
        if (!this->is_long())
        {
            if (!rhs.is_long()) {
                m_data = rhs.m_data;
            }
            else if (rhs.long_len() <= SHORT_STRING_CAP)
            {
                Traits::copy(this->short_bufp(), rhs.long_bufp(), rhs.long_len());
                this->short_len(rhs.long_len());
            }
            else grow_and_copy<true>(alloc, rhs);
        }
        else if (!rhs.is_long())
        {
            iutil::alloc_unchecked_delete(this->alloc(), long_bufp(), long_cap());
            m_data = rhs.m_data;
        }
        else if (!must_reallocate && rhs.long_len() <= this->long_cap())
        {
            Traits::copy(this->long_bufp(), rhs.long_bufp(), rhs.long_len());
            this->long_len(rhs.long_len());
        }
        else grow_and_copy<false>(alloc, rhs);
    }

private:
    union data
    {
        long_data long_;
        short_data short_;
    } 
    m_data;

    // legal even if long_data is not active.
    // https://en.cppreference.com/w/cpp/language/data_members#Standard_layout
    // https://stackoverflow.com/questions/34616086/
    inline bool is_long(void) const noexcept { return m_data.long_.is_long; }

    inline void is_long(bool value) noexcept
    {
        if (value) 
            m_data.long_.is_long = 1;
        else m_data.short_.is_long = 0;
    }

    inline void long_bufp(T* bufp) noexcept { m_data.long_.bufp = bufp; }
    inline void long_cap(std::size_t cap) noexcept { m_data.long_.cap = cap; }
    inline void long_len(std::size_t len) noexcept { m_data.long_.len = len; }
    inline void short_len(std::size_t len) noexcept
    {
        assert(len <= SHORT_STRING_CAP);
        m_data.short_.len = (unsigned char)len; 
    }

    inline T* long_bufp(void) const noexcept { return m_data.long_.bufp; }
    inline T* short_bufp(void) noexcept { return m_data.short_.buf; }
    inline const T* short_bufp(void) const noexcept { return m_data.short_.buf; }
    inline std::size_t long_cap(void) const noexcept { return m_data.long_.cap; }
    inline std::size_t long_len(void) const noexcept { return m_data.long_.len; }
    inline std::size_t short_len(void) const noexcept { return m_data.short_.len; }
};



// Fixed-size array stream buffer (modeled after C++23's std::basic_spanbuf).
template <typename CharT, typename Traits = std::char_traits<CharT>>
class basic_memspanbuf : public std::basic_streambuf<CharT, Traits>
{
public:
    using char_type = CharT;
    using traits_type = Traits;
    using int_type = typename Traits::int_type;
    using pos_type = typename Traits::pos_type;
    using off_type = typename Traits::off_type;

private:
    using base = std::basic_streambuf<CharT, Traits>;

public:
    basic_memspanbuf(CharT* src, std::streamsize size,
        std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) :
        base(), m_mode(mode)
    {
        if (!src) throw std::invalid_argument("Array pointer is null");
        setbuf(src, size);
    }

    basic_memspanbuf(memspan<CharT> src,
        std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) :
        basic_memspanbuf(src.begin, (std::streamsize)(src.end - src.begin), mode)
    {}

    template <std::size_t N>
    basic_memspanbuf(CharT(&src)[N],
        std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) :
        basic_memspanbuf(src, (std::streamsize)N, mode)
    {
        static_assert(N <= std::numeric_limits<std::streamsize>::max(), "Array is too large.");
    }

    basic_memspanbuf(basic_memspanbuf&&) = default;
    basic_memspanbuf(const basic_memspanbuf&) = delete;

    basic_memspanbuf& operator=(basic_memspanbuf&&) = default;
    basic_memspanbuf& operator=(const basic_memspanbuf&) = delete;

    // Get a span referencing the put area if 
    // ios_base::out is set in the open mode, or a
    // span referencing the entire array otherwise.
    inline memspan<CharT> data(void) const
    {
        if (m_mode & std::ios_base::out)
            return { this->pbase(), this->pptr() };
        else
            return { this->eback(), this->egptr() };
    }

protected:
    base* setbuf(CharT* src, std::streamsize size) final
    {
        if (m_mode & std::ios_base::in)
            this->setg(src, src, src + size);

        if (m_mode & std::ios_base::out)
        {
            this->setp(src, src + size);
            if (m_mode & std::ios_base::ate)
                safe_pbump(size);
        }
        return this;
    }

private:
    inline void safe_pbump(std::streamsize count)
    {
        constexpr int int_max = std::numeric_limits<int>::max();
        while (count != 0)
        {
            int to_bump = count > int_max ? int_max : (int)count;
            this->pbump(to_bump);
            count -= (std::streamsize)to_bump;
        }
    }

private:
    std::ios_base::openmode m_mode;
};

using memspanbuf = basic_memspanbuf<char>;

}}

#endif