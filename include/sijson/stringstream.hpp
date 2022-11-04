
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

// Input string stream.
template <typename CharT>
class basic_istrstream
{
private:
    using Traits = std::char_traits<CharT>;

public:
    using char_type = CharT;
    using streamsize_type = std::size_t;

public:
    basic_istrstream(memspan<const char_type> src) :
        m_begin(src.begin), m_cur(src.begin), m_end(src.end)
    {
#if SIJSON_PREFER_LOGIC_ERRORS
        if (!src.begin || !src.end)
            throw std::invalid_argument(std::string(__func__) + ": source is null.");
#else
        assert(src.begin && src.end);
#endif
    }

    basic_istrstream(const char_type* src, std::size_t size) :
        basic_istrstream(memspan<const char_type>(src, src + size))
    {}

    basic_istrstream(const char_type* src) :
        basic_istrstream(src, Traits::length(src))
    {}

#if SIJSON_HAS_STRING_VIEW
    basic_istrstream(std::basic_string_view<CharT, std::char_traits<CharT>> src) :
        basic_istrstream(src.data(), src.size())
    {}
#endif
    template <typename ...Ts>
    basic_istrstream(const std::basic_string<CharT, std::char_traits<CharT>, Ts...>& src) :
        basic_istrstream(src.data(), src.size())
    {}

    basic_istrstream(basic_istrstream&&) = default;
    basic_istrstream(const basic_istrstream&) = delete;

    basic_istrstream& operator=(basic_istrstream&&) = default;
    basic_istrstream& operator=(const basic_istrstream&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char_type peek(void) const noexcept { return *m_cur; }

    // Extract character. If end(), behavior is undefined.
    inline char_type take(void) noexcept { return *m_cur++; }

    // Get input position.
    inline streamsize_type inpos(void) const noexcept { return (streamsize_type)(m_cur - m_begin); }

    // True if the last operation reached the end of the stream.
    inline bool end(void) const noexcept { return m_cur == m_end; }

    // Jump to the beginning of the stream.
    inline void rewind(void) noexcept { m_cur = m_begin; }


    // Pointer to the first character in the stream.
    inline const char_type* inpbegin(void) const noexcept { return m_begin; }

    // Pointer to the next character returned by peek() or take().
    inline const char_type* inpcur(void) const noexcept { return m_cur; }

    // Pointer to one past the last character in the stream.
    inline const char_type* inpend(void) const noexcept { return m_end; }

    // Mark the next count characters in the stream as read.
    // If count is larger than the size of the remaining
    // data in the stream, the behavior is undefined.
    // After this call, inpos() increases by count.
    inline void incommit(streamsize_type count) noexcept { m_cur += count; }

private:
    const CharT* m_begin;
    const CharT* m_cur;
    const CharT* m_end;
};


// Output string stream.
template <
    typename CharT,
    typename Allocator = std::allocator<CharT>,
    typename TNullTerminated = void
>
class basic_ostrstream
{
private:
    using Traits = std::char_traits<CharT>;

public:
    using char_type = CharT;
    using streamsize_type = std::size_t;
    using allocator_type = Allocator;

    static constexpr bool is_null_terminated =
        std::is_same<TNullTerminated, tag::null_terminated>::value;

public:
    basic_ostrstream(streamsize_type init_capacity, 
        const Allocator& alloc = Allocator()
    ) :
        m_buf{ init_capacity + is_null_terminated, alloc }
    {
        init();
    }
    
    basic_ostrstream(const Allocator& alloc = Allocator()) :
        m_buf{ alloc }
    {
        init();
    }

    basic_ostrstream(basic_ostrstream&&) = default;
    basic_ostrstream(const basic_ostrstream& rhs) = default;

    basic_ostrstream& operator=(basic_ostrstream&&) = default;
    basic_ostrstream& operator=(const basic_ostrstream&) = default;

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

    // Put the same character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c, std::size_t count)
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

    // Synchronize with target (has no effect).
    inline void flush(void) {}

    // Get output position.
    inline streamsize_type outpos(void) const noexcept
    {
        return m_buf.length() - is_null_terminated;
    }

#if SIJSON_HAS_STRING_VIEW
    // Get view of underlying string.
    inline std::basic_string_view<CharT, std::char_traits<CharT>> 
        str_v(void) const noexcept { return { m_buf.pbegin(), outpos() }; }
#endif

    // Pointer to the first character in the stream.
    inline char_type* outpbegin(void) noexcept { return m_buf.pbegin(); }
    // Pointer to the first character in the stream.
    inline const char_type* outpbegin(void) const noexcept { return m_buf.pbegin(); }

    // Pointer to the next character modified by put().
    inline char_type* outpcur(void) noexcept { return m_buf.pend() - is_null_terminated; }
    // Pointer to the next character modified by put().
    inline const char_type* outpcur(void) const noexcept { return m_buf.pend() - is_null_terminated; }

    // Pointer to one past the last character of writable memory.
    inline char_type* outpend(void) noexcept 
    { return m_buf.pbegin() + m_buf.capacity() - is_null_terminated; }

    // Pointer to one past the last character of writable memory.
    inline const char_type* outpend(void) const noexcept 
    { return m_buf.pbegin() + m_buf.capacity() - is_null_terminated; }

    // Mark the next count characters in the stream as being initialized 
    // (i.e. the same as having been written by put() or put_n()). A call to this
    // is only required if you use the pointers outpbegin(), outpcur(), or outpend()
    // to modify stream data. If count is larger than the remaining reserved memory, 
    // the behavior is undefined. After this call, outpos() increases by count.
    inline void outcommit(streamsize_type count) noexcept 
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


// Output std::basic_string stream.
template <
    typename CharT,
    typename Allocator = std::allocator<CharT>
>
class basic_ostdstrstream
{
private:
    using Traits = std::char_traits<CharT>;
    using String = std::basic_string<CharT, Traits, Allocator>;

public:
    using char_type = CharT;
    using streamsize_type = typename String::size_type;
    using allocator_type = Allocator;
    using string_type = String;

    // pedantic check, ensure size_type is at least std::size_t
    static_assert(std::numeric_limits<streamsize_type>::max() >= 
        std::numeric_limits<std::size_t>::max(), "Platform not supported."); 
    
    static constexpr bool is_null_terminated = true;

public:
    basic_ostdstrstream(streamsize_type init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        m_str{ alloc }
    {
        m_str.reserve(init_capacity);
    }

    basic_ostdstrstream(const Allocator& alloc = Allocator()) :
        m_str{ alloc }
    {}

    basic_ostdstrstream(basic_ostdstrstream&&) = default;
    basic_ostdstrstream(const basic_ostdstrstream&) = default;

    basic_ostdstrstream& operator=(basic_ostdstrstream&&) = default;
    basic_ostdstrstream& operator=(const basic_ostdstrstream&) = default;

    // Put a character.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c) { m_str.push_back(c); }

    // Put the same character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c, std::size_t count) { m_str.append(count, c); }

    // Put characters from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_n(const char_type* str, std::size_t count) { m_str.append(str, count); }

    // Synchronize with target.
    inline void flush(void) {};

    // Get output position.
    inline streamsize_type outpos(void) const noexcept { return m_str.length(); }

#if SIJSON_HAS_STRING_VIEW
    // Get view of underlying string.
    inline std::basic_string_view<CharT, std::char_traits<CharT>>
        str_v(void) const noexcept { return m_str; }
#endif

    // Get underlying string.
    inline const string_type& str(void) const& noexcept { return m_str; }

    // Get rvalue reference to underlying string.
    // If moved from, stream is left in a valid but unspecified state.
    inline string_type&& str(void) && noexcept { return std::move(m_str); }


    // from C++11 onwards,
    // data() + i == std::addressof(operator[](i)) for every i in [0, size()]
    // https://en.cppreference.com/w/cpp/string/basic_string/data

    // Pointer to the first character in the stream.
    inline char_type* outpbegin(void) noexcept { return &m_str[0]; }

    // Pointer to the first character in the stream.
    inline const char_type* outpbegin(void) const noexcept { return m_str.data(); }

    // Pointer to the next character modified by put().
    inline char_type* outpcur(void) noexcept { return &m_str[m_str.size()]; }

    // Pointer to the next character modified by put().
    inline const char_type* outpcur(void) const noexcept { return &m_str[m_str.size()]; }

    // Cannot implement outpend(), outcommit() -> these require access to
    // std::string storage up to capacity(), which is undefined by the standard.

private:
    String m_str;
};


// Fixed size output string stream.
template <
    typename CharT,
    typename TThrowOnOverflow = tag::throw_on_overflow,
    typename TNullTerminated = void  
>
class basic_ostrspanstream
{
private:
    using Traits = std::char_traits<CharT>;

public:
    using char_type = CharT;
    using streamsize_type = std::size_t;

    static constexpr bool is_null_terminated =
        std::is_same<TNullTerminated, tag::null_terminated>::value;
    static constexpr bool throw_on_overflow =
        std::is_same<TThrowOnOverflow, tag::throw_on_overflow>::value;

public:
    basic_ostrspanstream(memspan<CharT> dest) :
        m_span(dest), m_cur(dest.begin)
    {
#if SIJSON_PREFER_LOGIC_ERRORS
        if (!dest.begin || !dest.end)
            throw std::invalid_argument(std::string(__func__) + ": dest is null.");

        if (is_null_terminated && dest.size() == 0)
            throw std::invalid_argument(std::string(__func__) + ": dest is too small.");
#else
        assert(dest.begin && dest.end && (!is_null_terminated || dest.size() > 0));
#endif
        if (is_null_terminated)
            *dest.begin = CharT();
    }

    template <std::size_t N>
    basic_ostrspanstream(CharT(&dest)[N]) :
        basic_ostrspanstream({ dest, dest + N })
    {}

    basic_ostrspanstream(CharT* dest, std::size_t size) :
        basic_ostrspanstream({ dest, dest + size })
    {}

    basic_ostrspanstream(basic_ostrspanstream&&) = default;
    basic_ostrspanstream(const basic_ostrspanstream&) = delete;

    basic_ostrspanstream& operator=(basic_ostrspanstream&&) = default;
    basic_ostrspanstream& operator=(const basic_ostrspanstream&) = delete;

    // Put a character. 
    // If avail() == 0, behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put(char_type c) 
        noexcept(!throw_on_overflow)
    {
        check_avail(1);

        Traits::assign(*m_cur, c);
        if (is_null_terminated)
            Traits::assign(m_cur[1], CharT());
        m_cur++;
    }

    // Put the same character multiple times.
    // If avail() < count, behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put(char_type c, std::size_t count)
        noexcept(!throw_on_overflow)
    {
        check_avail(count);

        Traits::assign(m_cur, count, c);
        if (is_null_terminated)
            Traits::assign(m_cur[count], CharT());
        m_cur += count;
    }

    // Put characters from an array.
    // If avail() < count, behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put_n(const char_type* str, std::size_t count)
        noexcept(!throw_on_overflow)
    {
        check_avail(count);

        Traits::copy(m_cur, str, count);
        if (is_null_terminated)
            Traits::assign(m_cur[count], CharT());
        m_cur += count;
    }

    // Synchronize with target (has no effect).
    inline void flush(void) {}

    // Get output position.
    inline std::size_t outpos(void) const noexcept
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
    inline std::basic_string_view<CharT, std::char_traits<CharT>>
        str_v(void) const noexcept { return { m_span.begin, outpos() }; }
#endif


    // Pointer to the first character in the stream.
    inline char_type* outpbegin(void) noexcept { return m_span.begin; }
    // Pointer to the first character in the stream.
    inline const char_type* outpbegin(void) const noexcept { return m_span.begin; }

    // Pointer to the next character modified by put().
    inline char_type* outpcur(void) noexcept { return m_cur; }
    // Pointer to the next character modified by put().
    inline const char_type* outpcur(void) const noexcept { return m_cur; }

    // Pointer to one past the last character of writable memory.
    inline char_type* outpend(void) noexcept { return m_span.end - is_null_terminated; }

    // Pointer to one past the last character of writable memory.
    inline const char_type* outpend(void) const noexcept { return m_span.end - is_null_terminated; }

    // Mark the next count characters in the stream as being initialized 
    // (i.e. the same as having been written by put() or put_n()). A call to this
    // is only required if you use the pointers outpbegin(), outpcur(), or outpend()
    // to modify stream data. If count is larger than the remaining reserved memory, 
    // the behavior is undefined. After this call, outpos() increases by count.
    inline void outcommit(streamsize_type count) noexcept
    {
        if (is_null_terminated)
            Traits::assign(m_cur[count], CharT());

        m_cur += count;
    }

private:
    inline void check_avail(std::size_t count) const
    {
        if (throw_on_overflow && count > avail())
            throw std::out_of_range("Stream exhausted");
    }

private:
    memspan<CharT> m_span;
    CharT* m_cur;
};


using istrstream = basic_istrstream<char>;
using wistrstream = basic_istrstream<wchar_t>;
using u16istrstream = basic_istrstream<char16_t>;
using u32istrstream = basic_istrstream<char32_t>;

using ostrstream = basic_ostrstream<char, std::allocator<char>, void>;
using wostrstream = basic_ostrstream<wchar_t, std::allocator<wchar_t>, void>;
using u16ostrstream = basic_ostrstream<char16_t, std::allocator<char16_t>, void>;
using u32ostrstream = basic_ostrstream<char32_t, std::allocator<char32_t>, void>;

using ocstrstream = basic_ostrstream<char, std::allocator<char>, tag::null_terminated>;
using wocstrstream = basic_ostrstream<wchar_t, std::allocator<wchar_t>, tag::null_terminated>;
using u16ocstrstream = basic_ostrstream<char16_t, std::allocator<char16_t>, tag::null_terminated>;
using u32ocstrstream = basic_ostrstream<char32_t, std::allocator<char32_t>, tag::null_terminated>;

using ostdstrstream = basic_ostdstrstream<char, std::allocator<char>>;
using wostdstrstream = basic_ostdstrstream<wchar_t, std::allocator<wchar_t>>;
using u16ostdstrstream = basic_ostdstrstream<char16_t, std::allocator<char16_t>>;
using u32ostdstrstream = basic_ostdstrstream<char32_t, std::allocator<char32_t>>;

using ostrspanstream = basic_ostrspanstream<char, tag::throw_on_overflow, void>;
using wostrspanstream = basic_ostrspanstream<wchar_t, tag::throw_on_overflow, void>;
using u16ostrspanstream = basic_ostrspanstream<char16_t, tag::throw_on_overflow, void>;
using u32ostrspanstream = basic_ostrspanstream<char32_t, tag::throw_on_overflow, void>;

using ocstrspanstream = basic_ostrspanstream<char, tag::throw_on_overflow, tag::null_terminated>;
using wocstrspanstream = basic_ostrspanstream<wchar_t, tag::throw_on_overflow, tag::null_terminated>;
using u16ocstrspanstream = basic_ostrspanstream<char16_t, tag::throw_on_overflow, tag::null_terminated>;
using u32ocstrspanstream = basic_ostrspanstream<char32_t, tag::throw_on_overflow, tag::null_terminated>;

using unchecked_ostrspanstream = basic_ostrspanstream<char, void, void>;
using unchecked_wostrspanstream = basic_ostrspanstream<wchar_t, void, void>;
using unchecked_u16ostrspanstream = basic_ostrspanstream<char16_t, void, void>;
using unchecked_u32ostrspanstream = basic_ostrspanstream<char32_t, void, void>;

using unchecked_ocstrspanstream = basic_ostrspanstream<char, void, tag::null_terminated>;
using unchecked_wocstrspanstream = basic_ostrspanstream<wchar_t, void, tag::null_terminated>;
using unchecked_u16ocstrspanstream = basic_ostrspanstream<char16_t, void, tag::null_terminated>;
using unchecked_u32ocstrspanstream = basic_ostrspanstream<char32_t, void, tag::null_terminated>;

#if __cpp_char8_t
using u8istrstream = basic_istrstream<char8_t>;
using u8ostrstream = basic_ostrstream<char8_t, std::allocator<char8_t>, void>;
using u8ocstrstream = basic_ostrstream<char8_t, std::allocator<char8_t>, tag::null_terminated>;
using u8ostdstrstream = basic_ostdstrstream<char8_t, std::allocator<char8_t>>;
using u8ostrspanstream = basic_ostrspanstream<char8_t, tag::throw_on_overflow, void>;
using u8ocstrspanstream = basic_ostrspanstream<char8_t, tag::throw_on_overflow, tag::null_terminated>;
using unchecked_u8ostrspanstream = basic_ostrspanstream<char8_t, void, void>;
using unchecked_u8ocstrspanstream = basic_ostrspanstream<char8_t, void, tag::null_terminated>;
#endif

}

#endif