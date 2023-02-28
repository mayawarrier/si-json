
#ifndef SIJSON_STRINGSTREAM_HPP
#define SIJSON_STRINGSTREAM_HPP

#include <cstddef>
#include <memory>
#include <cstring>
#include <string>
#include <stdexcept>
#include <type_traits>

#if SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif

#include "internal/config.hpp"
#include "internal/util.hpp"
#include "internal/buffers.hpp"

#include "core.hpp"


namespace sijson {

//
// Input string.
// ------------------------
// Implements ContiguousInput.
// ------------------------
//
template <typename CharT>
class basic_in_str
{
private:
    using Traits = std::char_traits<CharT>;

public:
    using char_type = CharT;
    using size_type = std::size_t;
    using input_kind = tag::io_contiguous;

public:
    basic_in_str(memspan<const char_type> src) :
        m_begin(src.begin), m_cur(src.begin), m_end(src.end)
    {
#if SIJSON_LOGIC_ERRORS
        if (!src.begin || !src.end)
            throw std::invalid_argument(SIJSON_SRCLOC + 
                iutil::nameof<basic_in_str<CharT>>() + ": src is null.");
#else
        assert(src.begin && src.end);
#endif
    }

    basic_in_str(const char_type* src, std::size_t size) :
        basic_in_str(memspan<const char_type>(src, src + size))
    {}

    basic_in_str(const char_type* src) :
        basic_in_str(src, Traits::length(src))
    {}

#if SIJSON_HAS_STRING_VIEW
    basic_in_str(std::basic_string_view<CharT, std::char_traits<CharT>> src) :
        basic_in_str(src.data(), src.size())
    {}
#endif
    template <typename ...Ts>
    basic_in_str(const std::basic_string<CharT, std::char_traits<CharT>, Ts...>& src) :
        basic_in_str(src.data(), src.size())
    {}

    basic_in_str(basic_in_str&&) = default;
    basic_in_str(const basic_in_str&) = delete;

    basic_in_str& operator=(basic_in_str&&) = default;
    basic_in_str& operator=(const basic_in_str&) = delete;


    // Get char. If end(), behavior is undefined.
    inline char_type peek(void) const noexcept { return *m_cur; }

    // Extract char. If end(), behavior is undefined.
    inline char_type take(void) noexcept { return *m_cur++; }

    // True if input has run out of characters.
    inline bool end(void) const noexcept { return m_cur == m_end; }

    // Jump to the beginning.
    inline void rewind(void) noexcept { m_cur = m_begin; }

    // Get input position.
    inline size_type ipos(void) const noexcept
    {
        return (size_type)(m_cur - m_begin);
    }

    // Pointer to the first char.
    inline const char_type* ipbeg(void) const noexcept { return m_begin; }

    // Pointer to the current char.
    inline const char_type* ipcur(void) const noexcept { return m_cur; }

    // Pointer to one past the last char.
    inline const char_type* ipend(void) const noexcept { return m_end; }

    // Commit the next count bytes (i.e. mark them as 'read'.
    // Required only if you use ipbeg(), ipcur() or ipend() to read data).
    // Increments the input position by count.
    inline void icommit(size_type count) noexcept { m_cur += count; }

private:
    const CharT* m_begin;
    const CharT* m_cur;
    const CharT* m_end;
};


//
// Output string.
// ------------------------
// Implements ContiguousOutput.
// ------------------------
// Supports custom allocators and fancy pointers.
// Optimizes for short strings.
// ------------------------
//
template <
    typename CharT,
    typename Allocator = std::allocator<CharT>,
    typename TNullTerminated = void
>
class basic_out_str
{
private:
    using Traits = std::char_traits<CharT>;
#if SIJSON_HAS_STRING_VIEW
    using StringView = std::basic_string_view<CharT, Traits>;
#endif
    using StdString = std::basic_string<CharT, Traits, Allocator>;

public:
    using char_type = CharT;
    using size_type = std::size_t;
    using allocator_type = Allocator;
    using output_kind = tag::io_contiguous;

    static constexpr bool is_null_terminated =
        std::is_same<TNullTerminated, tag::null_terminated>::value;

public:
    basic_out_str(size_type capacity, 
        const Allocator& alloc = Allocator()
    ) :
        m_buf{ capacity + is_null_terminated, alloc }
    {
        init();
    }
    
    basic_out_str(const Allocator& alloc = Allocator()) :
        m_buf{ alloc }
    {
        init();
    }

    basic_out_str(basic_out_str&&) = default;
    basic_out_str(const basic_out_str&) = default;

    basic_out_str& operator=(basic_out_str&&) = default;
    basic_out_str& operator=(const basic_out_str&) = default;

    // Put a character.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c)
    {
        m_buf.reserve(m_buf.length() + 1);

        if (is_null_terminated)
        {
            Traits::assign(m_buf[m_buf.length() - 1], c);
            Traits::assign(m_buf[m_buf.length()], CharT());
        }
        else Traits::assign(m_buf[m_buf.length()], c);
        
        m_buf.commit(1);
    }

    // Put a character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_f(char_type c, size_type count)
    {
        m_buf.reserve(m_buf.length() + count);

        if (is_null_terminated)
        {
            Traits::assign(m_buf.pend() - 1, count, c);
            Traits::assign(m_buf[m_buf.length() + count - 1], CharT());
        }
        else Traits::assign(m_buf.pend(), count, c);

        m_buf.commit(count);
    }

    // Put characters from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_n(const char_type* str, std::size_t count)
    {
        m_buf.reserve(m_buf.length() + count);

        if (is_null_terminated)
        {
            Traits::copy(m_buf.pend() - 1, str, count);
            Traits::assign(m_buf[m_buf.length() + count - 1], CharT());
        }
        else Traits::copy(m_buf.pend(), str, count);
            
        m_buf.commit(count);
    }

    // No effect.
    inline void flush(void) {}

    // Get output position.
    inline size_type opos(void) const noexcept
    {
        return m_buf.length() - is_null_terminated;
    }

#if SIJSON_HAS_STRING_VIEW
    // Get view of string.
    inline StringView view(void) const noexcept 
    { 
        return { m_buf.pbegin(), opos() }; 
    }
#endif

    // Copy string into a std::basic_string.
    inline StdString stdstr(void) const
    {
        return { m_buf.pbegin(), opos(), m_buf.get_allocator() };
    }

    // Increase # reserved chars to at least new_capacity.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void reserve(size_type new_capacity)
    {
        m_buf.reserve(new_capacity + is_null_terminated);
    }

    // Pointer to the first char.
    inline char_type* opbeg(void) noexcept { return m_buf.pbegin(); }

    // Pointer to the first char.
    inline const char_type* opbeg(void) const noexcept { return m_buf.pbegin(); }

    // Pointer to the next writable char.
    inline char_type* opcur(void) noexcept { return m_buf.pend() - is_null_terminated; }

    // Pointer to the next writable char.
    inline const char_type* opcur(void) const noexcept { return m_buf.pend() - is_null_terminated; }

    // Pointer to one past the last char of reserved memory.
    inline char_type* opend(void) noexcept 
    { 
        return m_buf.pbegin() + m_buf.capacity() - is_null_terminated; 
    }

    // Pointer to one past the last char of reserved memory.
    inline const char_type* opend(void) const noexcept 
    { 
        return m_buf.pbegin() + m_buf.capacity() - is_null_terminated;
    }

    // Commit the next count bytes (i.e. mark them as 'written to'.
    // Required only if you use opbeg(), opcur() or opend() to write data).
    // Increments the output position by count.
    inline void ocommit(size_type count) noexcept 
    {
        if (is_null_terminated)
            Traits::assign(m_buf[m_buf.length() + count - 1], CharT());

        m_buf.commit(count);
    }

private:
    inline void init(void) noexcept
    {
        if (is_null_terminated)
        {
            // okay, min_capacity is at least 1
            m_buf[0] = CharT();
            m_buf.commit(1);
        }
    }
private:
    internal::strbuffer<CharT, Traits, Allocator> m_buf;
};

//
// Output std::basic_string.
// ------------------------
// Supports custom allocators.
// May not support fancy pointers or implement SSO (check your std library).
// ------------------------
//
template <
    typename CharT,
    typename Allocator = std::allocator<CharT>
>
class basic_out_stdstr
{
private:
    using Traits = std::char_traits<CharT>;
    using String = std::basic_string<CharT, Traits, Allocator>;
#if SIJSON_HAS_STRING_VIEW
    using StringView = std::basic_string_view<CharT, Traits>;
#endif

public:
    using char_type = CharT;
    using size_type = typename String::size_type;
    using allocator_type = Allocator;
    using output_kind = tag::io_basic;
    
    static constexpr bool is_null_terminated = true;

public:
    basic_out_stdstr(size_type capacity,
        const Allocator& alloc = Allocator()
    ) :
        m_str{ alloc }
    {
        m_str.reserve(capacity);
    }

    basic_out_stdstr(const Allocator& alloc = Allocator()) :
        m_str{ alloc }
    {}

    basic_out_stdstr(basic_out_stdstr&&) = default;
    basic_out_stdstr(const basic_out_stdstr&) = default;

    basic_out_stdstr& operator=(basic_out_stdstr&&) = default;
    basic_out_stdstr& operator=(const basic_out_stdstr&) = default;

    // Put a character.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c) { m_str.push_back(c); }

    // Put a character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_f(char_type c, size_type count) { m_str.append(count, c); }

    // Put characters from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_n(const char_type* str, std::size_t count) { m_str.append(str, count); }

    // No effect.
    inline void flush(void) noexcept {};

    // Get output position.
    inline size_type opos(void) const noexcept { return m_str.length(); }

#if SIJSON_HAS_STRING_VIEW
    // Get view of string.
    inline StringView view(void) const noexcept { return m_str; }
#endif

    // Get underlying string.
    inline const String& str(void) const& noexcept { return m_str; }

    // Get rvalue reference to underlying string.
    // If moved from, stream is left in a valid but unspecified state.
    inline String&& str(void) && noexcept { return std::move(m_str); }

    // from C++11 onwards,
    // data() + i == std::addressof(operator[](i)) for every i in [0, size()]
    // https://en.cppreference.com/w/cpp/string/basic_string/data

    // Pointer to the first char.
    inline char_type* opbeg(void) noexcept { return &m_str[0]; }

    // Pointer to the first char.
    inline const char_type* opbeg(void) const noexcept { return m_str.data(); }

    // Pointer to the next writable char.
    inline char_type* opcur(void) noexcept { return &m_str[m_str.size()]; }

    // Pointer to the next writable char.
    inline const char_type* opcur(void) const noexcept { return &m_str[m_str.size()]; }

    // Cannot implement opend(), ocommit(). These require access to
    // std::string storage up to capacity(), which is undefined.

private:
    String m_str;
};

//
// Fixed size output string.
// ------------------------
// Implements ContiguousOutput.
// ------------------------
//
template <
    typename CharT,
    typename TThrowOnOverflow = void,
    typename TNullTerminated = void  
>
class basic_out_strspan
{
private:
    using Traits = std::char_traits<CharT>;
#if SIJSON_HAS_STRING_VIEW
    using StringView = std::basic_string_view<CharT, Traits>;
#endif
    using ThisType = basic_out_strspan<CharT, TThrowOnOverflow, TNullTerminated>;

public:
    using char_type = CharT;
    using size_type = std::size_t;
    using output_kind = tag::io_contiguous;

    static constexpr bool is_null_terminated =
        std::is_same<TNullTerminated, tag::null_terminated>::value;
    static constexpr bool throws_on_overflow =
        std::is_same<TThrowOnOverflow, tag::throw_on_overflow>::value;

private:
    using throw_on_overflow_t = std::integral_constant<bool, throws_on_overflow>;

public:
    basic_out_strspan(memspan<CharT> dest) :
        m_span(dest), m_cur(dest.begin)
    {
#if SIJSON_LOGIC_ERRORS
        if (!dest.begin || !dest.end)
            throw std::invalid_argument(SIJSON_SRCLOC +
                iutil::nameof<ThisType>() + ": dest is null.");

        if (is_null_terminated && dest.size() == 0)
            throw std::invalid_argument(SIJSON_SRCLOC +
                iutil::nameof<ThisType>() + ": dest is too small.");
#else
        assert(dest.begin && dest.end && (!is_null_terminated || dest.size() > 0));
#endif
        if (is_null_terminated)
            *dest.begin = CharT();
    }

    template <std::size_t N>
    basic_out_strspan(CharT(&dest)[N]) :
        basic_out_strspan({ dest, dest + N })
    {}

    basic_out_strspan(CharT* dest, std::size_t size) :
        basic_out_strspan({ dest, dest + size })
    {}

    basic_out_strspan(basic_out_strspan&&) = default;
    basic_out_strspan(const basic_out_strspan&) = delete;

    basic_out_strspan& operator=(basic_out_strspan&&) = default;
    basic_out_strspan& operator=(const basic_out_strspan&) = delete;

    // Put a character. 
    // If avail() == 0, the behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put(char_type c) 
        noexcept(!throws_on_overflow)
    {
        check_avail(1, throw_on_overflow_t{});

        Traits::assign(*m_cur, c);
        if (is_null_terminated)
            Traits::assign(m_cur[1], CharT());
        m_cur++;
    }

    // Put a character multiple times.
    // If avail() < count, the behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put_f(char_type c, size_type count)
        noexcept(!throws_on_overflow)
    {
        check_avail(count, throw_on_overflow_t{});

        Traits::assign(m_cur, count, c);
        if (is_null_terminated)
            Traits::assign(m_cur[count], CharT());
        m_cur += count;
    }

    // Put characters from an array.
    // If avail() < count, the behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put_n(const char_type* str, std::size_t count)
        noexcept(!throws_on_overflow)
    {
        check_avail(count, throw_on_overflow_t{});

        Traits::copy(m_cur, str, count);
        if (is_null_terminated)
            Traits::assign(m_cur[count], CharT());
        m_cur += count;
    }

    // No effect.
    inline void flush(void) noexcept {}

    // Get output position.
    inline std::size_t opos(void) const noexcept
    {
        return (std::size_t)(m_cur - m_span.begin);
    }

    // Number of bytes available for writing.
    inline std::size_t avail(void) const noexcept
    {
        return (std::size_t)(m_span.end - m_cur) - is_null_terminated;
    }

#if SIJSON_HAS_STRING_VIEW
    // Get view of underlying string.
    inline StringView view(void) const noexcept 
    {
        return { m_span.begin, opos() }; 
    }
#endif

    // Throws if ThrowOnOverflow is true and the
    // span is less than new_capacity in size.
    inline void reserve(size_type new_capacity) const 
        noexcept(!throws_on_overflow)
    {
        do_reserve(new_capacity, throw_on_overflow_t{});
    }

    // Pointer to the first char.
    inline char_type* opbeg(void) noexcept { return m_span.begin; }

    // Pointer to the first character.
    inline const char_type* opbeg(void) const noexcept { return m_span.begin; }

    // Pointer to the next writable char.
    inline char_type* opcur(void) noexcept { return m_cur; }

    // Pointer to the next writable char.
    inline const char_type* opcur(void) const noexcept { return m_cur; }

    // Pointer to one past the last char of writable memory.
    inline char_type* opend(void) noexcept { return m_span.end - is_null_terminated; }

    // Pointer to one past the last character of writable memory.
    inline const char_type* opend(void) const noexcept { return m_span.end - is_null_terminated; }

    // Commit the next count bytes (i.e. mark them as 'written to'.
    // Required only if you use opbeg(), opcur() or opend() to write data).
    // Increments the output position by count.
    inline void ocommit(size_type count) noexcept
    {
        if (is_null_terminated)
            Traits::assign(m_cur[count], CharT());

        m_cur += count;
    }

private:
    inline void do_reserve(size_type new_cap, std::true_type) const
    {
        if (new_cap > m_span.size() - is_null_terminated)
            throw std::runtime_error(SIJSON_SRCLOC "Output exhausted");
    }
    inline void do_reserve(size_type, std::false_type) const noexcept {}

    inline void check_avail(std::size_t count, std::true_type) const
    {
        if (count > avail())
            throw std::runtime_error(SIJSON_SRCLOC "Output exhausted");
    }
    inline void check_avail(std::size_t, std::false_type) const noexcept { }

private:
    memspan<CharT> m_span;
    CharT* m_cur;
};


using in_str = basic_in_str<char>;
using in_wstr = basic_in_str<wchar_t>;
using in_u16str = basic_in_str<char16_t>;
using in_u32str = basic_in_str<char32_t>;

using out_str = basic_out_str<char, std::allocator<char>, void>;
using out_wstr = basic_out_str<wchar_t, std::allocator<wchar_t>, void>;
using out_u16str = basic_out_str<char16_t, std::allocator<char16_t>, void>;
using out_u32str = basic_out_str<char32_t, std::allocator<char32_t>, void>;

using out_cstr = basic_out_str<char, std::allocator<char>, tag::null_terminated>;
using out_wcstr = basic_out_str<wchar_t, std::allocator<wchar_t>, tag::null_terminated>;
using out_u16cstr = basic_out_str<char16_t, std::allocator<char16_t>, tag::null_terminated>;
using out_u32cstr = basic_out_str<char32_t, std::allocator<char32_t>, tag::null_terminated>;

using out_stdstr = basic_out_stdstr<char, std::allocator<char>>;
using out_wstdstr = basic_out_stdstr<wchar_t, std::allocator<wchar_t>>;
using out_u16stdstr = basic_out_stdstr<char16_t, std::allocator<char16_t>>;
using out_u32stdstr = basic_out_stdstr<char32_t, std::allocator<char32_t>>;

using out_strspan = basic_out_strspan<char, void, void>;
using out_wstrspan = basic_out_strspan<wchar_t, void, void>;
using out_u16strspan = basic_out_strspan<char16_t, void, void>;
using out_u32strspan = basic_out_strspan<char32_t, void, void>;

using out_cstrspan = basic_out_strspan<char, void, tag::null_terminated>;
using out_wcstrspan = basic_out_strspan<wchar_t, void, tag::null_terminated>;
using out_u16cstrspan = basic_out_strspan<char16_t, void, tag::null_terminated>;
using out_u32cstrspan = basic_out_strspan<char32_t, void, tag::null_terminated>;

using out_strspan_s = basic_out_strspan<char, tag::throw_on_overflow, void>;
using out_wstrspan_s = basic_out_strspan<wchar_t, tag::throw_on_overflow, void>;
using out_u16strspan_s = basic_out_strspan<char16_t, tag::throw_on_overflow, void>;
using out_u32strspan_s = basic_out_strspan<char32_t, tag::throw_on_overflow, void>;

using out_cstrspan_s = basic_out_strspan<char, tag::throw_on_overflow, tag::null_terminated>;
using out_wcstrspan_s = basic_out_strspan<wchar_t, tag::throw_on_overflow, tag::null_terminated>;
using out_u16cstrspan_s = basic_out_strspan<char16_t, tag::throw_on_overflow, tag::null_terminated>;
using out_u32cstrspan_s = basic_out_strspan<char32_t, tag::throw_on_overflow, tag::null_terminated>;

#if __cpp_char8_t
using in_u8str = basic_in_str<char8_t>;
using out_u8str = basic_out_str<char8_t, std::allocator<char8_t>, void>;
using out_u8cstr = basic_out_str<char8_t, std::allocator<char8_t>, tag::null_terminated>;
using out_u8stdstr = basic_out_stdstr<char8_t, std::allocator<char8_t>>;
using out_u8strspan = basic_out_strspan<char8_t, void, void>;
using out_u8cstrspan = basic_out_strspan<char8_t, void, tag::null_terminated>;
using out_u8strspan_s = basic_out_strspan<char8_t, tag::throw_on_overflow, tag::null_terminated>;
using out_u8cstrspan_s = basic_out_strspan<char8_t, tag::throw_on_overflow, tag::null_terminated>;
#endif

}

#endif