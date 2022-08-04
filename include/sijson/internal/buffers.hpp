
#ifndef SIJSON_INTERNAL_BUFFERS_HPP
#define SIJSON_INTERNAL_BUFFERS_HPP

#include <cstddef>
#include <cstring>
#include <limits>
#include <memory>
#include <type_traits>
#include <ios>
#include <streambuf>
#include <string>
#include <algorithm>
#include <utility>
#include <stdexcept>

#include "config.hpp"
#include "common.hpp"
#include "util.hpp"
#include "util_memory.hpp"


namespace sijson {
namespace internal {

// Dynamically-allocated resizable contiguous memory buffer.
//
// Elements are default-initialized only if alloc.construct() is non-trivial.
// All member functions have a strong exception guarantee if alloc.destroy() is noexcept.
// 
// https://en.cppreference.com/w/cpp/named_req/Allocator
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p1072r10.html
// 
template <typename T, 
    typename Allocator = std::allocator<T>>
class buffer : iutil::alloc_holder<Allocator>
{
private:
    using Allocholder = iutil::alloc_holder<Allocator>;
    using Altraits = std::allocator_traits<Allocator>;

public:
    using pointer = typename Altraits::pointer;
    using const_pointer = typename Altraits::const_pointer;
    using iterator = pointer;
    using const_iterator = const_pointer;

public:
    buffer(std::size_t init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        Allocholder(alloc),
        m_size(iutil::recommend_allocn_size<T>(this->alloc(), init_capacity)), 
        m_bufp(iutil::alloc_new(this->alloc(), m_size))
    {}

    buffer(const Allocator& alloc = Allocator()) :
        buffer(16, alloc)
    {}

    buffer(buffer&& rhs) :
        Allocholder(rhs), m_size(rhs.m_size),
        m_bufp(rhs.m_bufp)
    {
        rhs.m_bufp = nullptr;
    }

    buffer(const buffer& rhs) :
        Allocholder(rhs), m_size(rhs.m_size),
        m_bufp(iutil::alloc_new(this->alloc(), rhs.m_size))
    {
        iutil::scope_guard<Allocator> guard(this->alloc(), m_bufp, m_size);
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

    inline void reserve(std::size_t atleast_cap)
    {
        if (atleast_cap <= capacity())
            return;

        grow_and_copy(this->alloc(), atleast_cap, m_bufp, m_size);
    }    

    inline iterator begin(void) noexcept { return m_bufp; }
    inline iterator end(void) noexcept { return m_bufp + m_size; }

    inline const_iterator begin(void) const noexcept { return m_bufp; }
    inline const_iterator end(void) const noexcept { return m_bufp + m_size; }

    inline T* pbegin(void) noexcept { return iutil::to_address(begin()); }
    inline T* pend(void) noexcept { return iutil::to_address(end()); }

    inline const T* pbegin(void) const noexcept { return iutil::to_address(begin()); }
    inline const T* pend(void) const noexcept { return iutil::to_address(end()); }
   
    inline T& operator[](std::size_t index) noexcept { return m_bufp[index]; }
    inline const T& operator[](std::size_t index) const noexcept { return m_bufp[index]; }

    inline memspan<T> data(void) noexcept { return { pbegin(), pend() }; }
    inline memspan<const T> data(void) const noexcept { return { pbegin(), pend() }; }

    ~buffer(void)
    {
        iutil::alloc_delete(this->alloc(), m_bufp, m_size);
    }

private:
    // Assumes this != &rhs.
    template <bool MustReallocate = false>
    inline void copy_assign(const buffer& rhs, Allocator& alloc)
    {
        if (MustReallocate || m_size != rhs.m_size)
        {
            reallocate_and_copy(alloc, rhs.m_size, rhs.m_bufp, rhs.m_size);
        } 
        else std::copy(rhs.m_bufp, rhs.m_bufp + rhs.m_size, m_bufp);
    }

    inline void reallocate_and_copy(Allocator& alloc, std::size_t new_cap, const_pointer src, std::size_t len)
    {
        assert(new_cap >= len && new_cap != m_size);

        pointer bufp = iutil::alloc_new(alloc, new_cap);
        iutil::scope_guard<Allocator> guard(alloc, bufp, new_cap);

        std::copy(src, src + len, bufp);
        iutil::alloc_unchecked_delete(this->alloc(), m_bufp, m_size);

        m_bufp = bufp;
        m_size = new_cap;

        guard.disable();
    }

    inline void grow_and_copy(Allocator& alloc, std::size_t atleast_cap, const_pointer src, std::size_t len)
    {
        auto new_cap = iutil::recommend_vector_growth<T>(alloc, m_size, atleast_cap);
        reallocate_and_copy(alloc, new_cap, src, len);
    }

private:
    std::size_t m_size;
    pointer m_bufp;
};


template <typename T, typename Allocator>
struct strdata_types
{
    using long_pointer_t = iutil::alloc_ptr_t<Allocator>;

    using long_pointer_is_pod = iutil::is_pod<long_pointer_t>;

    using long_pointer_storage_t = 
        typename std::conditional<
            long_pointer_is_pod::value,
            long_pointer_t,
            iutil::pod_storage<long_pointer_t>
        >::type;

    struct long_data
    {
        unsigned char is_long : 1;
        std::size_t cap;
        std::size_t len;
        long_pointer_storage_t bufp;
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
};

template <typename T, typename Allocator,
    bool = strdata_types<T, Allocator>::long_pointer_is_pod::value
>
struct strdata_base : strdata_types<T, Allocator>
{   
    using StrdataTypes = strdata_types<T, Allocator>;

    using long_data = typename StrdataTypes::long_data;
    using short_data = typename StrdataTypes::short_data;
    using long_pointer_t = typename StrdataTypes::long_pointer_t;

    union data
    {
        long_data long_;
        short_data short_;
    }
    m_data;

    // activate as short
    strdata_base(void) noexcept { m_data.short_.is_long = 0; }
    strdata_base(const data& d) noexcept : m_data(d) {}

    inline void set_is_long(bool value) noexcept
    {
        if (value)
            m_data.long_.is_long = 1;
        else m_data.short_.is_long = 0;
    }

    inline void long_bufp(long_pointer_t p) noexcept { m_data.long_.bufp = p; }

    inline long_pointer_t long_bufp(void) const noexcept { return m_data.long_.bufp; }
};

// Fancy pointer support.
template <typename T, typename Allocator>
struct strdata_base<T, Allocator, false> : strdata_types<T, Allocator>
{
    using StrdataTypes = strdata_types<T, Allocator>;

    using long_data = typename StrdataTypes::long_data;
    using short_data = typename StrdataTypes::short_data;
    using long_pointer_t = typename StrdataTypes::long_pointer_t;

    union data
    {
        // activate as short
        data(void) noexcept { short_.is_long = 0; }

        data& operator=(const data& rhs) noexcept
        {
            if (this != &rhs)
            {
                if (rhs.long_.is_long)
                {
                    if (!long_.is_long)
                        long_.bufp.construct();

                    long_.is_long = rhs.long_.is_long;
                    long_.cap = rhs.long_.cap;
                    long_.len = rhs.long_.len;
                    long_.bufp.get() = rhs.long_.bufp.get();
                }
                else {
                    if (long_.is_long)
                        long_.bufp.destroy();
                    short_ = rhs.short_;
                }
            }
            return *this;
        }

        data(const data& rhs) noexcept 
        {
            if (rhs.long_.is_long)
                long_.bufp.construct(rhs.long_.bufp.get());
            else short_ = rhs.short_;
        }

        ~data(void) noexcept
        {
            if (long_.is_long)
                long_.bufp.destroy();
        }

        long_data long_;
        short_data short_;
    }
    m_data;

    strdata_base(void) noexcept : m_data() {}
    strdata_base(const data& d) noexcept : m_data(d) {}

    inline void set_is_long(bool value) noexcept
    {
        if (value)
        {
            if (!m_data.long_.is_long)
            {
                m_data.long_.is_long = 1;
                m_data.long_.bufp.construct();
            }
        }
        else if (m_data.long_.is_long)
        {
            m_data.long_.bufp.destroy();
            m_data.short_.is_long = 0;
        }
    }

    inline void long_bufp(const long_pointer_t& p) noexcept { m_data.long_.bufp.get() = p; }

    inline void long_bufp(long_pointer_t&& p) noexcept { m_data.long_.bufp.get() = std::move(p); }

    inline long_pointer_t long_bufp(void) const noexcept { return m_data.long_.bufp.get(); }
};

template <typename T, typename Allocator>
struct strdata : strdata_base<T, Allocator>
{
    using StrdataBase = strdata_base<T, Allocator>;
    using data = typename StrdataBase::data;

    strdata(void) noexcept : StrdataBase() {}
    strdata(const data& d) noexcept : StrdataBase(d) {}

    // Allowed even if long_data is inactive, iff
    // all members are POD (Common subsequence rule).
    // https://en.cppreference.com/w/cpp/language/data_members#Standard_layout
    inline bool is_long(void) const noexcept { return this->m_data.long_.is_long; }

    inline T* short_bufp(void) noexcept { return this->m_data.short_.buf; }
    inline const T* short_bufp(void) const noexcept { return this->m_data.short_.buf; }

    inline std::size_t long_cap(void) const noexcept { return this->m_data.long_.cap; }
    inline std::size_t long_len(void) const noexcept { return this->m_data.long_.len; }

    inline void long_cap(std::size_t cap) noexcept { this->m_data.long_.cap = cap; }
    inline void long_len(std::size_t len) noexcept { this->m_data.long_.len = len; }

    inline void short_len(std::size_t len) noexcept
    {
        assert(len <= this->SHORT_STRING_CAP);
        this->m_data.short_.len = (unsigned char)len;
    }
    inline std::size_t short_len(void) const noexcept { return this->m_data.short_.len; }
};


// Buffer with short-string optimization.
//
// Elements are default-initialized only if alloc.construct() is non-trivial.
// All member functions have a strong exception guarantee if alloc.destroy() is noexcept.
//
template <typename T, 
    typename Traits = std::char_traits<T>, 
    typename Allocator = std::allocator<T>
>
class strbuffer : iutil::alloc_holder<Allocator>, strdata<T, Allocator>
{
private:
    using Allocholder = iutil::alloc_holder<Allocator>;
    using Strdata = strdata<T, Allocator>;
    using Altraits = std::allocator_traits<Allocator>;

public:
    static_assert(iutil::is_pod<T>::value, "T must be a plain-old-data type.");

    using pointer = typename Altraits::pointer;
    using const_pointer = typename Altraits::const_pointer;
    using iterator = T*;
    using const_iterator = const T*;

public:
    // Initial capacity should include space for null-terminator (if any).
    strbuffer(std::size_t init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        Allocholder(alloc)
    {
        bool sso = init_capacity <= this->SHORT_STRING_CAP;
        this->set_is_long(!sso);

        if (sso) this->short_len(0);
        else
        {
            auto new_cap = iutil::recommend_allocn_size<T>(this->alloc(), init_capacity);
            this->long_bufp(iutil::alloc_new(this->alloc(), new_cap));
            this->long_cap(new_cap);
            this->long_len(0);
        }
    }

    strbuffer(const Allocator& alloc = Allocator()) :
        strbuffer(this->SHORT_STRING_CAP, alloc)
    {}

    strbuffer(strbuffer&& rhs) : 
        Allocholder(rhs), Strdata(rhs.m_data)
    {
        if (rhs.is_long())
            rhs.long_bufp(nullptr);
    }

    strbuffer(const strbuffer& rhs) : 
        Allocholder(rhs)
    {
        if (!rhs.is_long())
            this->m_data.short_ = rhs.m_data.short_;
        else 
            copy_assign(rhs, this->alloc());
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
                    iutil::alloc_unchecked_delete(this->alloc(), this->long_bufp(), this->long_len());

                // steal
                this->m_data = rhs.m_data;
                if (rhs.is_long()) 
                    rhs.long_bufp(nullptr);
            }
            // else copy (see Allocator requirements)
            else copy_assign(rhs, this->alloc());

            iutil::move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        }
        return *this;
    }

    inline std::size_t length(void) const noexcept 
    { return this->is_long() ? this->long_len() : this->short_len(); }

    inline std::size_t capacity(void) const noexcept 
    { return this->is_long() ? this->long_cap() : this->SHORT_STRING_CAP; }

    // Guaranteed to be at least 1.
    constexpr std::size_t min_capacity(void) const noexcept { return this->SHORT_STRING_CAP; }

    // New capacity should include space for null-terminator (if any).
    inline void reserve(std::size_t atleast_cap)
    {
        if (atleast_cap <= capacity())
            return;

        grow(atleast_cap);
    }

    // Mark available capacity as under use.
    inline void commit(std::size_t count)
    {
        assert(length() + count <= capacity());
        this->is_long() ? 
            this->long_len(this->long_len() + count) : 
            this->short_len(this->short_len() + count);
    }

    inline iterator begin(void) noexcept 
    { 
        return this->is_long() ? 
            iutil::to_address(this->long_bufp()) : this->short_bufp(); 
    }

    inline iterator end(void) noexcept
    { 
        return this->is_long() ? 
            iutil::to_address(this->long_bufp()) + this->long_len() :
            this->short_bufp() + this->short_len(); 
    }

    inline const_iterator begin(void) const noexcept
    {
        return this->is_long() ?
            iutil::to_address(this->long_bufp()) : this->short_bufp();
    }

    inline const_iterator end(void) const noexcept
    {
        return this->is_long() ?
            iutil::to_address(this->long_bufp()) + this->long_len() :
            this->short_bufp() + this->short_len();
    }

    inline T* pbegin(void) noexcept { return iutil::to_address(begin()); }
    inline T* pend(void) noexcept { return iutil::to_address(end()); }

    inline const T* pbegin(void) const noexcept { return iutil::to_address(begin()); }
    inline const T* pend(void) const noexcept { return iutil::to_address(end()); }

    inline T& operator[](std::size_t index) noexcept { return begin()[index]; }
    inline const T& operator[](std::size_t index) const noexcept { return begin()[index]; }

    inline memspan<T> data(void) noexcept { return { pbegin(), pend() }; }
    inline memspan<const T> data(void) const noexcept { return { pbegin(), pend() }; }

    ~strbuffer(void)
    {
        if (this->is_long())
            iutil::alloc_delete(this->alloc(), this->long_bufp(), this->long_cap());
    }

private:
    template <bool IsShort>
    inline void grow_and_copy(Allocator& alloc, std::size_t atleast_cap, const T* src, std::size_t len)
    {
        assert(atleast_cap >= len && atleast_cap > capacity());

        auto new_cap = iutil::recommend_vector_growth<T>(alloc, capacity(), atleast_cap);

        pointer bufp = iutil::alloc_new(alloc, new_cap);
        iutil::scope_guard<Allocator> guard(alloc, bufp, new_cap);

        Traits::copy(iutil::to_address(bufp), src, len);

        if (!IsShort)
            iutil::alloc_unchecked_delete(this->alloc(), this->long_bufp(), this->long_cap());
        else this->set_is_long(true);

        this->long_bufp(bufp);
        this->long_cap(new_cap);
        this->long_len(len);

        guard.disable();
    }

    // Assumes this != &rhs.
    template <bool IsShort>
    inline void grow_and_copy(Allocator& alloc, const strbuffer& rhs)
    {
        grow_and_copy<IsShort>(alloc, rhs.long_len(), iutil::to_address(rhs.long_bufp()), rhs.long_len());
    }

    inline void grow(std::size_t atleast_cap)
    {
        if (!this->is_long())
            grow_and_copy<true>(this->alloc(), atleast_cap, this->short_bufp(), this->short_len());
        else
            grow_and_copy<false>(this->alloc(), atleast_cap, iutil::to_address(this->long_bufp()), this->long_len());
    }

    // Assumes this != &rhs.
    template <bool MustReallocate = false>
    inline void copy_assign(const strbuffer& rhs, Allocator& alloc)
    {
        if (!this->is_long())
        {
            if (!rhs.is_long()) {
                this->m_data.short_ = rhs.m_data.short_;
            }
            else if (rhs.long_len() <= this->SHORT_STRING_CAP)
            {
                Traits::copy(this->short_bufp(), iutil::to_address(rhs.long_bufp()), rhs.long_len());
                this->short_len(rhs.long_len());
            }
            else grow_and_copy<true>(alloc, rhs);
        }
        else if (!rhs.is_long())
        {
            iutil::alloc_unchecked_delete(this->alloc(), this->long_bufp(), this->long_cap());
            this->m_data = rhs.m_data;
        }
        else if (!MustReallocate && rhs.long_len() <= this->long_cap())
        {
            Traits::copy(iutil::to_address(this->long_bufp()), iutil::to_address(rhs.long_bufp()), rhs.long_len());
            this->long_len(rhs.long_len());
        }
        else grow_and_copy<false>(alloc, rhs);
    }
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