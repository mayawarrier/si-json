
#ifndef SIJSON_MEMORYSTREAM_HPP
#define SIJSON_MEMORYSTREAM_HPP

#include <cstddef>
#include <cstring>
#include <memory>
#include <stdexcept>

#include "common.hpp"
#include "internal/buffers.hpp"

namespace sijson {

// Input memory stream.
class imemstream
{
public:
    imemstream(memspan<const char> span) :
        m_begin(span.begin), m_cur(span.begin), m_end(span.end)
    {
        if (!span.begin || !span.end)
            throw std::invalid_argument("Source is null");
    }

    imemstream(const char* src, std::size_t size) :
        imemstream({ src, src + size })
    {}

    imemstream(imemstream&&) = default;
    imemstream(const imemstream&) = delete;

    imemstream& operator=(imemstream&&) = default;
    imemstream& operator=(const imemstream&) = delete;

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
class basic_omemstream
{
public:
    using allocator_type = Allocator;

public:
    basic_omemstream(std::size_t init_capacity,
        const Allocator& alloc = Allocator()
    ) :
        m_buf{ init_capacity, alloc }, m_pos(0)
    {}

    basic_omemstream(const Allocator& alloc = Allocator()) :
        m_buf{ alloc }, m_pos(0)
    {}

    basic_omemstream(basic_omemstream&&) = default;
    basic_omemstream(const basic_omemstream& rhs) = default;

    basic_omemstream& operator=(basic_omemstream&&) = default;
    basic_omemstream& operator=(const basic_omemstream&) = default;

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

using omemstream = basic_omemstream<std::allocator<char>>;

}

#endif