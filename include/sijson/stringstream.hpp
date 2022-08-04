
#ifndef SIJSON_STRINGSTREAM_HPP
#define SIJSON_STRINGSTREAM_HPP

#include <cstddef>
#include <memory>
#include <cstring>
#include <string>
#include <stdexcept>
#include <type_traits>

#include "internal/config.hpp"
#include "internal/util.hpp"
#include "internal/buffers.hpp"

#include "common.hpp"
#include "memorystream.hpp"

#if SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif


namespace sijson {

// Input string stream.
class istrstream : public imemstream
{
public:
#if SIJSON_HAS_STRING_VIEW
    template <typename Traits>
    istrstream(std::basic_string_view<char, Traits> src) :
        imemstream(src.data(), src.size())
    {}
#endif
    istrstream(memspan<const char> span) :
        imemstream(span)
    {}

    istrstream(const char* src, std::size_t size) :
        imemstream(src, size)
    {}

    istrstream(istrstream&&) = default;
    istrstream(const istrstream&) = delete;

    istrstream& operator=(istrstream&&) = default;
    istrstream& operator=(const istrstream&) = delete;
};

// Input C-string stream.
class icstrstream
{
public:
    icstrstream(const char* src) :
        m_begin(src), m_cur(src)
    {
        if (!src) 
            throw std::invalid_argument("Source is null");
    }

    icstrstream(icstrstream&&) = default;
    icstrstream(const icstrstream&) = delete;

    icstrstream& operator=(icstrstream&&) = default;
    icstrstream& operator=(const icstrstream&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char peek(void) const noexcept { return *m_cur; }

    // Extract character. If end(), behavior is undefined.
    inline char take(void) noexcept { return *m_cur++; }

    // Get input position.
    inline std::size_t inpos(void) const noexcept { return (std::size_t)(m_cur - m_begin); }

    // True if the last operation completed by reaching the end of the stream.
    inline bool end(void) const noexcept { return *m_cur == '\0'; }

    // Jump to the beginning of the stream.
    inline void rewind(void) noexcept { m_cur = m_begin; }

private:
    const char* m_begin;
    const char* m_cur;
};


// Input std::basic_string stream.
class istdstrstream : public imemstream
{
public:
    template <typename ...Ts>
    istdstrstream(const std::basic_string<char, Ts...>& str) :
        imemstream(str.c_str(), str.length())
    {}

    istdstrstream(istdstrstream&&) = default;
    istdstrstream(const istdstrstream&) = delete;

    istdstrstream& operator=(istdstrstream&&) = default;
    istdstrstream& operator=(const istdstrstream&) = delete;
};


// Output string stream.
template <
    typename Traits = std::char_traits<char>,
    typename Allocator = std::allocator<char>,
    bool NullTerminated = true
>
class basic_ostrstream
{
public:
    using traits_type = Traits;
    using allocator_type = Allocator;
    static constexpr bool is_null_terminated = NullTerminated;

public:
    basic_ostrstream(std::size_t init_capacity, 
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
    inline void put(char c)
    {
        m_buf.reserve(m_buf.length() + 1);

        if (is_null_terminated)
        {
            Traits::assign(m_buf[m_buf.length() - 1], c);
            Traits::assign(m_buf[m_buf.length()], '\0');
        }
        else Traits::assign(m_buf[m_buf.length()], c);
        
        m_buf.commit(1);
    }

    // Put the same character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char c, std::size_t count)
    {
        m_buf.reserve(m_buf.length() + count);

        if (is_null_terminated)
        {
            Traits::assign(m_buf.pend() - 1, count, c);
            Traits::assign(m_buf[m_buf.length() + count - 1], '\0');
        }
        else Traits::assign(m_buf.pend(), count, c);

        m_buf.commit(count);
    }

    // Put characters from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void putn(const char* str, std::size_t count)
    {
        m_buf.reserve(m_buf.length() + count);

        if (is_null_terminated)
        {
            Traits::copy(m_buf.pend() - 1, str, count);
            Traits::assign(m_buf[m_buf.length() + count - 1], '\0');
        }
        else Traits::copy(m_buf.pend(), str, count);
            
        m_buf.commit(count);
    }

    // Synchronize with target.
    inline void flush(void) {}

    // Get output position.
    inline std::size_t outpos(void) const noexcept
    {
        return m_buf.length() - is_null_terminated;
    }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<char> outdata(void) noexcept 
    {
        return { m_buf.pbegin(), m_buf.pend() - is_null_terminated };
    }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<const char> outdata(void) const noexcept 
    {
        return { m_buf.pbegin(), m_buf.pend() - is_null_terminated };
    }

    // Get underlying string (immutable).
    // Result is null-terminated if is_null_terminated is true.
    inline const char* str(void) const noexcept { return m_buf.pbegin(); }

private:
    inline void init(void) noexcept
    {
        if (is_null_terminated)
        {
            // okay, min_capacity is at least 1
            m_buf[0] = '\0';
            m_buf.commit(1);
        }
    }
private:
    internal::strbuffer<char, Traits, Allocator> m_buf;
};


// Output std::basic_string stream.
template <
    typename Traits = std::char_traits<char>,
    typename Allocator = std::allocator<char>>
class basic_ostdstrstream
{
public:
    using traits_type = Traits;
    using allocator_type = Allocator;
    using string_type = std::basic_string<char, Traits, Allocator>;
    static constexpr bool is_null_terminated = true;

public:
    basic_ostdstrstream(std::size_t init_capacity,
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
    inline void put(char c) { m_str.push_back(c); }

    // Put the same character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char c, std::size_t count) { m_str.append(count, c); }

    // Put characters from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void putn(const char* str, std::size_t count) { m_str.append(str, count); }

    // Synchronize with target.
    inline void flush(void) {};

    // Get output position.
    inline std::size_t outpos(void) const noexcept { return m_str.length(); }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<char> outdata(void) noexcept
    {
        // from C++11 onwards,
        // data() + i == std::addressof(operator[](i)) for every i in [0, size()]
        // https://en.cppreference.com/w/cpp/string/basic_string/data
        return { &m_str[0], &m_str[m_str.size()] };
    }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<const char> outdata(void) const noexcept
    {
        return { m_str.data(), m_str.data() + m_str.size() };
    }

    // Get underlying string (immutable).
    inline const string_type& str(void) const& noexcept { return m_str; }

    // Get string move-constructed from underlying string.
    // On return, stream is left in a valid but unspecified state.
    inline string_type str(void) && noexcept 
    {
        string_type str(std::move(m_str));
        return str;
    }

private:
    string_type m_str;
};


// Fixed size output string stream.
template <
    typename Traits = std::char_traits<char>,
    bool NullTerminated = true,
    bool ThrowOnOverflow = true
>
class basic_ostrspanstream
{
public:
    using traits_type = Traits;
    static constexpr bool is_null_terminated = NullTerminated;
    static constexpr bool throw_on_overflow = ThrowOnOverflow;

public:
    basic_ostrspanstream(memspan<char> span) :
        m_span(span), m_cur(span.begin)
    {
        if (!span.begin || !span.end || (is_null_terminated && span.size() < 1))
            throw std::invalid_argument("Invalid span");

        if (is_null_terminated)
            *span.begin = '\0';
    }

    template <std::size_t N>
    basic_ostrspanstream(char(&src)[N]) :
        basic_ostrspanstream({ src, src + N })
    {}

    basic_ostrspanstream(char* src, std::size_t size) :
        basic_ostrspanstream({ src, src + size })
    {}

    basic_ostrspanstream(basic_ostrspanstream&&) = default;
    basic_ostrspanstream(const basic_ostrspanstream&) = delete;

    basic_ostrspanstream& operator=(basic_ostrspanstream&&) = default;
    basic_ostrspanstream& operator=(const basic_ostrspanstream&) = delete;

    // Put a character. 
    // If avail() == 0, behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put(char c) 
        noexcept(!ThrowOnOverflow)
    {
        check_avail(1);

        Traits::assign(*m_cur, c);
        if (is_null_terminated)
            Traits::assign(m_cur[1], '\0');
        m_cur++;
    }

    // Put the same character multiple times.
    // If avail() < count, behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void put(char c, std::size_t count)
        noexcept(!ThrowOnOverflow)
    {
        check_avail(count);

        Traits::assign(m_cur, count, c);
        if (is_null_terminated)
            Traits::assign(m_cur[count], '\0');
        m_cur += count;
    }

    // Put characters from an array.
    // If avail() < count, behavior is undefined
    // unless ThrowOnOverflow is true.
    inline void putn(const char* str, std::size_t count) 
        noexcept(!ThrowOnOverflow)
    {
        check_avail(count);

        // safer to move, str may overlap with input span
        Traits::move(m_cur, str, count);
        if (is_null_terminated)
            Traits::assign(m_cur[count], '\0');
        m_cur += count;
    }

    // Synchronize with target.
    inline void flush(void) {}

    // Number of bytes available for writing.
    inline std::size_t avail(void) const noexcept
    {
        return (std::size_t)(m_span.end - m_cur) - is_null_terminated;
    }

    // Get output position.
    inline std::size_t outpos(void) const noexcept
    {
        return (std::size_t)(m_cur - m_span.begin);
    }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<char> outdata(void) noexcept 
    {
        return { m_span.begin, m_cur }; 
    }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<const char> outdata(void) const noexcept 
    { 
        return { m_span.begin, m_cur }; 
    }

    // Get underlying string (immutable).
    // Result is null-terminated if is_null_terminated is true.
    inline const char* str(void) const noexcept { return m_span.begin; }

private:
    inline void check_avail(std::size_t count) const
    {
        if (throw_on_overflow && count > avail())
            throw std::out_of_range("Stream exhausted");
    }

private:
    memspan<char> m_span;
    char* m_cur;
};


using ocstrstream = basic_ostrstream<std::char_traits<char>, std::allocator<char>, null_terminated_t<true>{}>;
using ostrstream = basic_ostrstream<std::char_traits<char>, std::allocator<char>, null_terminated_t<false>{}>;
using ostdstrstream = basic_ostdstrstream<std::char_traits<char>, std::allocator<char>>;

using ocstrspanstream = basic_ostrspanstream<std::char_traits<char>, null_terminated_t<true>{}>;
using ostrspanstream = basic_ostrspanstream<std::char_traits<char>, null_terminated_t<false>{}>;

using unchecked_ocstrspanstream = basic_ostrspanstream<std::char_traits<char>, null_terminated_t<true>{}, throw_on_overflow_t<false>{}>;
using unchecked_ostrspanstream = basic_ostrspanstream<std::char_traits<char>, null_terminated_t<false>{}, throw_on_overflow_t<false>{}>;

}

#endif