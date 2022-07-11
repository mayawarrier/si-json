
#ifndef SIJSON_MEMORYSTREAM_HPP
#define SIJSON_MEMORYSTREAM_HPP

#include <cstddef>
#include <cstring>
#include <memory>
#include <stdexcept>

#include "common.hpp"
#include "internal/buffers.hpp"

namespace json {

// Input memory stream.
class imstream
{
public:
    imstream(memspan<const char> span) :
        m_begin(span.begin), m_cur(span.begin), m_end(span.end)
    {
        if (!span.begin || !span.end)
            throw std::invalid_argument("Source is null");
    }

    imstream(const char* src, std::size_t size) :
        imstream({ src, src + size })
    {}

    imstream(imstream&&) = default;
    imstream(const imstream&) = delete;

    imstream& operator=(imstream&&) = default;
    imstream& operator=(const imstream&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char peek(void) const noexcept { return *m_cur; }

    // Extract character. If end(), behavior is undefined.
    inline char take(void) noexcept { return *m_cur++; }

    // Get input position.
    inline std::size_t inpos(void) const noexcept { return (std::size_t)(m_cur - m_begin); }

    // Get total number of chars available for input.
    inline std::size_t inlength(void) const noexcept { return (std::size_t)(m_end - m_begin); }

    // Span of the entire input data i.e. from position 0 to inlength().
    inline memspan<const char> indata(void) const noexcept { return { m_begin, m_end }; }

    // True if the last operation reached the end of the stream.
    inline bool end(void) const noexcept { return m_cur == m_end; }

    // Jump to the beginning of the stream.
    inline void rewind(void) noexcept { m_cur = m_begin; }

private:
    const char* m_begin;
    const char* m_cur;
    const char* m_end;
};


// Output memory stream.
template <typename Allocator = std::allocator<char>>
class basic_omstream
{
public:
    using allocator_type = Allocator;

public:
    basic_omstream(std::size_t init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        m_buf{ init_capacity, alloc }, m_pos(0)
    {}

    basic_omstream(const Allocator& alloc = Allocator()) :
        m_buf{ alloc }, m_pos(0)
    {}

    basic_omstream(basic_omstream&&) = default;
    basic_omstream(const basic_omstream& rhs) = default;

    basic_omstream& operator=(basic_omstream&&) = default;
    basic_omstream& operator=(const basic_omstream&) = default;

    // Put a character.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char c)
    {
        m_buf.reserve(m_pos + 1);
        m_buf[m_pos++] = c;
    }

    // Put the same character multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char c, std::size_t count)
    {
        m_buf.reserve(m_pos + count);
        std::memset(&m_buf[m_pos], c, count);
        m_pos += count;
    }

    // Put characters from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void putn(const char* str, std::size_t count)
    {
        m_buf.reserve(m_pos + count);
        std::memcpy(&m_buf[m_pos], str, count);
        m_pos += count;
    }

    // Synchronize with target.
    inline void flush(void) {}

    // Get output position.
    inline std::size_t outpos(void) const noexcept { return m_pos; }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<char> outdata(void) noexcept
    {
        return { m_buf.begin(), m_buf.begin() + m_pos };
    }

    // Span of the underlying storage from 0 to outpos().
    // Span may be invalidated if a non-const reference to
    // the stream is passed to a function or if any non-const
    // member functions are called on the stream.
    inline memspan<const char> outdata(void) const noexcept
    {
        return { m_buf.begin(), m_buf.begin() + m_pos };
    }

private:
    internal::buffer<char, Allocator> m_buf;
    std::size_t m_pos;
};

using omstream = basic_omstream<std::allocator<char>>;

}

#endif