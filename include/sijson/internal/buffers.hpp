
#ifndef SIJSON_INTERNAL_BUFFERS_HPP
#define SIJSON_INTERNAL_BUFFERS_HPP

#include <cstddef>
#include <cassert>
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
#include "core.hpp"
#include "util.hpp"
#include "util_memory.hpp"


namespace sijson {
namespace internal {

// Dynamically-allocated resizable buffer.
//
// Elements are default-initialized only if alloc.construct() is non-trivial.
// All member functions have a strong exception guarantee if alloc.destroy() is noexcept.
// 
// https://en.cppreference.com/w/cpp/named_req/Allocator
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p1072r10.html
// 
template <typename T, 
    typename Allocator = std::allocator<T>
>
class buffer : private iutil::alloc_holder<Allocator>
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
        Allocholder(iutil::in_place, alloc),
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
            reallocate_and_copy(alloc, rhs.m_size, rhs.m_bufp, rhs.m_size);
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
struct strdata_base
{
    using long_pointer_t = iutil::alloc_ptr_t<Allocator>;

    using is_long_pointer_pod = iutil::is_pod<long_pointer_t>;

    using long_pointer_storage_t = 
        typename std::conditional<
            is_long_pointer_pod::value,
            long_pointer_t,
            iutil::aligned_storage_for<long_pointer_t>
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

    union data
    {
        long_data long_;
        short_data short_;
    };
    
    // activate short
    strdata_base(void) noexcept { m_data.short_.is_long = 0; }

    // Copies object representation only!
    strdata_base(const data& d) noexcept : m_data(d) {}

    // default would always copy object representation
    strdata_base& operator=(const strdata_base&) = delete;
    strdata_base& operator=(strdata_base&&) = delete;

    // All members must be POD to access is_long from an inactive member
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

    inline void copy_short(const strdata_base& rhs) noexcept { this->m_data.short_ = rhs.m_data.short_; }

protected:
    data m_data;
};

template <typename T, typename Allocator,
    bool = strdata_base<T, Allocator>::is_long_pointer_pod::value
>
struct strdata : strdata_base<T, Allocator>
{   
    using StrdataBase = strdata_base<T, Allocator>;
    using long_pointer_t = typename StrdataBase::long_pointer_t;
    
    strdata(void) noexcept : StrdataBase() {}
    strdata(const strdata& rhs) noexcept : StrdataBase(rhs.m_data) {}

    inline strdata& operator=(const strdata& rhs) noexcept { this->m_data = rhs.m_data; }

    inline void set_is_long(bool value) noexcept
    {
        if (value)
            this->m_data.long_.is_long = 1; // activate long_
        else this->m_data.short_.is_long = 0; // activate short_
    }

    inline void long_bufp(long_pointer_t p) noexcept { this->m_data.long_.bufp = p; }
    inline long_pointer_t long_bufp(void) const noexcept { return this->m_data.long_.bufp; }
};


// Fancy pointer support.
template <typename T, typename Allocator>
struct strdata<T, Allocator, false> : strdata_base<T, Allocator>
{
    using StrdataBase = strdata_base<T, Allocator>;
    using long_pointer_t = typename StrdataBase::long_pointer_t;

    strdata(void) noexcept : StrdataBase() {}

    strdata(const strdata& rhs) noexcept : StrdataBase()
    {
        if (rhs.is_long())
        {
            this->m_data.long_.bufp.construct(rhs.m_data.long_.bufp.get());
            this->m_data.long_.cap = rhs.m_data.long_.cap;
            this->m_data.long_.len = rhs.m_data.long_.len;
        }
        else this->m_data.short_ = rhs.m_data.short_;
    }

    ~strdata(void) noexcept
    {
        if (this->is_long())
            this->m_data.long_.bufp.destroy();
    }

    inline strdata& operator=(const strdata& rhs) noexcept
    {
        if (this != &rhs)
        {
            if (rhs.is_long())
            {
                if (!this->is_long())
                    this->m_data.long_.bufp.construct();

                this->m_data.long_.is_long = rhs.m_data.long_.is_long;
                this->m_data.long_.cap = rhs.m_data.long_.cap;
                this->m_data.long_.len = rhs.m_data.long_.len;
                this->m_data.long_.bufp.get() = rhs.m_data.long_.bufp.get();
            }
            else {
                if (this->is_long())
                    this->m_data.long_.bufp.destroy();
                this->m_data.short_ = rhs.m_data.short_;
            }
        }
        return *this;
    }

    inline void set_is_long(bool value) noexcept
    {
        if (value)
        {
            if (!this->is_long())
            {
                this->m_data.long_.is_long = 1;
                this->m_data.long_.bufp.construct();
            }
        }
        else if (this->is_long())
        {
            this->m_data.long_.bufp.destroy();
            this->m_data.short_.is_long = 0;
        }
    }

    inline void long_bufp(const long_pointer_t& p) noexcept { this->m_data.long_.bufp.get() = p; }
    inline void long_bufp(long_pointer_t&& p) noexcept { this->m_data.long_.bufp.get() = std::move(p); }

    inline long_pointer_t long_bufp(void) const noexcept { return this->m_data.long_.bufp.get(); }
};


// buffer<> with short-string optimization.
//
// Elements are default-initialized only if alloc.construct() is non-trivial.
// All member functions have a strong exception guarantee if alloc.destroy() is noexcept.
//
template <typename T, 
    typename Traits = std::char_traits<T>, 
    typename Allocator = std::allocator<T>
>
class strbuffer : private iutil::alloc_holder<Allocator>
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
        Allocholder(iutil::in_place, alloc)
    {
        bool sso = init_capacity <= Strdata::SHORT_STRING_CAP;
        this->m_str.set_is_long(!sso);

        if (sso) 
            this->m_str.short_len(0);
        else
        {
            auto new_cap = iutil::recommend_allocn_size<T>(this->alloc(), init_capacity);
            this->m_str.long_bufp(iutil::alloc_new(this->alloc(), new_cap));
            this->m_str.long_cap(new_cap);
            this->m_str.long_len(0);
        }
    }

    strbuffer(const Allocator& alloc = Allocator()) :
        strbuffer(Strdata::SHORT_STRING_CAP, alloc)
    {}

    strbuffer(strbuffer&& rhs) : 
        Allocholder(rhs), m_str(rhs.m_str)
    {
        if (rhs.m_str.is_long())
            rhs.m_str.long_bufp(nullptr);
    }

    strbuffer(const strbuffer& rhs) : 
        Allocholder(rhs)
    {
        if (!rhs.m_str.is_long())
            this->m_str.copy_short(rhs.m_str);
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
                if (this->m_str.is_long())
                    iutil::alloc_unchecked_delete(this->alloc(), 
                        this->m_str.long_bufp(), this->m_str.long_len());

                // steal
                this->m_str = rhs.m_str;
                if (rhs.m_str.is_long()) 
                    rhs.m_str.long_bufp(nullptr);
            }
            // else copy (see Allocator requirements)
            else copy_assign(rhs, this->alloc());

            iutil::move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        }
        return *this;
    }

    inline std::size_t length(void) const noexcept 
    { return this->m_str.is_long() ? this->m_str.long_len() : this->m_str.short_len(); }

    inline void set_length(std::size_t new_length) noexcept
    {
        assert(new_length <= capacity());
        this->m_str.is_long() ?
            this->m_str.long_len(new_length) :
            this->m_str.short_len(new_length);
    }

    inline std::size_t capacity(void) const noexcept 
    { return this->m_str.is_long() ? this->m_str.long_cap() : Strdata::SHORT_STRING_CAP; }

    // Guaranteed to be at least 1.
    constexpr std::size_t min_capacity(void) const noexcept { return Strdata::SHORT_STRING_CAP; }

    // New capacity should include space for null-terminator (if any).
    inline void reserve(std::size_t atleast_cap)
    {
        if (atleast_cap <= capacity())
            return;

        grow(atleast_cap);
    }

    // Mark the next count chars as under use.
    inline void commit(std::size_t count) 
    {
        assert(length() + count <= capacity());
        this->m_str.is_long() ?
            this->m_str.long_len(this->m_str.long_len() + count) :
            this->m_str.short_len(this->m_str.short_len() + count);
    }

    inline iterator begin(void) noexcept 
    { 
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) : this->m_str.short_bufp();
    }

    inline iterator end(void) noexcept
    { 
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) + this->m_str.long_len() :
            this->m_str.short_bufp() + this->m_str.short_len();
    }

    inline const_iterator begin(void) const noexcept
    {
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) : this->m_str.short_bufp();
    }

    inline const_iterator end(void) const noexcept
    {
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) + this->m_str.long_len() :
            this->m_str.short_bufp() + this->m_str.short_len();
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
        if (this->m_str.is_long())
            iutil::alloc_delete(this->alloc(), this->m_str.long_bufp(), this->m_str.long_cap());
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
            iutil::alloc_unchecked_delete(this->alloc(), this->m_str.long_bufp(), this->m_str.long_cap());
        else this->m_str.set_is_long(true);

        this->m_str.long_bufp(bufp);
        this->m_str.long_cap(new_cap);
        this->m_str.long_len(len);

        guard.disable();
    }

    // Assumes this != &rhs.
    template <bool IsShort>
    inline void grow_and_copy(Allocator& alloc, const strbuffer& rhs)
    {
        grow_and_copy<IsShort>(alloc, rhs.m_str.long_len(), 
            iutil::to_address(rhs.m_str.long_bufp()), rhs.m_str.long_len());
    }

    inline void grow(std::size_t atleast_cap)
    {
        if (!this->m_str.is_long())
            grow_and_copy<true>(this->alloc(), atleast_cap, 
                this->m_str.short_bufp(), this->m_str.short_len());
        else
            grow_and_copy<false>(this->alloc(), atleast_cap, 
                iutil::to_address(this->m_str.long_bufp()), this->m_str.long_len());
    }

    // Assumes this != &rhs.
    template <bool MustReallocate = false>
    inline void copy_assign(const strbuffer& rhs, Allocator& alloc)
    {
        if (!this->m_str.is_long())
        {
            if (!rhs.m_str.is_long()) {
                this->m_str.copy_short(rhs.m_str);
            }
            else if (rhs.m_str.long_len() <= Strdata::SHORT_STRING_CAP)
            {
                Traits::copy(this->m_str.short_bufp(), 
                    iutil::to_address(rhs.m_str.long_bufp()), rhs.m_str.long_len());
                this->m_str.short_len(rhs.m_str.long_len());
            }
            else grow_and_copy<true>(alloc, rhs);
        }
        else if (!rhs.m_str.is_long())
        {
            iutil::alloc_unchecked_delete(this->alloc(), this->m_str.long_bufp(), this->m_str.long_cap());
            this->m_str = rhs.m_str;
        }
        else if (!MustReallocate && rhs.m_str.long_len() <= this->m_str.long_cap())
        {
            Traits::copy(iutil::to_address(this->m_str.long_bufp()), 
                iutil::to_address(rhs.m_str.long_bufp()), rhs.m_str.long_len());
            this->m_str.long_len(rhs.m_str.long_len());
        }
        else grow_and_copy<false>(alloc, rhs);
    }

private:
    Strdata m_str;
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
            this->setp(src, src + size);

        return this;
    }

private:
    std::ios_base::openmode m_mode;
};

using memspanbuf = basic_memspanbuf<char>;

}}

#endif