
#ifndef SIJSON_MEMORYSTREAM_HPP
#define SIJSON_MEMORYSTREAM_HPP

#include <cstddef>
#include <cassert>
#include <cstring>
#include <memory>
#include <stdexcept>

#include "core.hpp"
#include "internal/util.hpp"
#include "internal/buffers.hpp"

namespace sijson {

// Input (contiguous) memory stream.
// Char type is sijson::byte (std::byte if available, otherwise unsigned char).
class imemstream
{
public:
    using char_type = sijson::byte;
    using streamsize_type = std::size_t;

public:
    imemstream(memspan<const sijson::byte> src) :
        m_begin(src.begin), m_cur(src.begin), m_end(src.end)
    {
#if SIJSON_PREFER_LOGIC_ERRORS
        if (!src.begin || !src.end)
            throw std::invalid_argument(std::string(__func__) + ": source is null.");
#else
        assert(src.begin && src.end);
#endif
    }

    imemstream(const sijson::byte* src, std::size_t size) :
        imemstream({ src, src + size })
    {}

    imemstream(imemstream&&) = default;
    imemstream(const imemstream&) = delete;

    imemstream& operator=(imemstream&&) = default;
    imemstream& operator=(const imemstream&) = delete;

    // Get byte. If end(), behavior is undefined.
    inline char_type peek(void) const noexcept { return *m_cur; }

    // Extract byte. If end(), behavior is undefined.
    inline char_type take(void) noexcept { return *m_cur++; }

    // Get input position.
    inline streamsize_type inpos(void) const noexcept { return (streamsize_type)(m_cur - m_begin); }

    // True if the last operation reached the end of the stream.
    inline bool end(void) const noexcept { return m_cur == m_end; }   

    // Jump to the beginning of the stream.
    inline void rewind(void) noexcept { m_cur = m_begin; }


    // Pointer to the first byte in the stream.
    inline const char_type* inpbegin(void) const noexcept { return m_begin; }

    // Pointer to the next byte returned by peek() or take().
    inline const char_type* inpcur(void) const noexcept { return m_cur; }

    // Pointer to one past the last byte in the stream.
    inline const char_type* inpend(void) const noexcept { return m_end; }

    // Mark the next count characters in the stream as read.
    // If count is larger than the size of the remaining
    // data in the stream, the behavior is undefined.
    // After this call, inpos() increases by count.
    inline void incommit(streamsize_type count) noexcept { m_cur += count; }

private:
    const char_type* m_begin;
    const char_type* m_cur;
    const char_type* m_end;
};


// Output (contiguous) memory stream.
// Char type is sijson::byte (std::byte if available, otherwise unsigned char).
// This does not optimize for short strings, unlike sijson::ostrstream.
template <typename Allocator = std::allocator<sijson::byte>>
class basic_omemstream
{
public:
    using char_type = sijson::byte;
    using streamsize_type = std::size_t;
    using allocator_type = Allocator;

public:
    basic_omemstream(streamsize_type init_capacity,
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

    // Put a byte.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c)
    {
        m_buf.reserve(m_pos + 1);
        m_buf[m_pos++] = c;
    }

    // Put the same byte multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c, std::size_t count)
    {
        m_buf.reserve(m_pos + count);
        std::memset(&m_buf[m_pos], c, count);
        m_pos += count;
    }

    // Put bytes from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void putn(const char_type* arr, std::size_t count)
    {
        m_buf.reserve(m_pos + count);
        std::memcpy(&m_buf[m_pos], arr, count);
        m_pos += count;
    }

    // Synchronize with target (has no effect).
    inline void flush(void) {}

    // Get output position.
    inline streamsize_type outpos(void) const noexcept { return m_pos; }


    // Pointer to the first byte in the stream.
    inline char_type* outpbegin(void) noexcept { return m_buf.pbegin(); }
    // Pointer to the first byte in the stream.
    inline const char_type* outpbegin(void) const noexcept { return m_buf.pbegin(); }

    // Pointer to the next byte modified by put().
    inline char_type* outpcur(void) noexcept { return m_buf.pbegin() + m_pos; }
    // Pointer to the next byte modified by put().
    inline const char_type* outpcur(void) const noexcept { return m_buf.pbegin() + m_pos; }

    // Pointer to one past the last byte of reserved memory.
    inline char_type* outpend(void) noexcept { return m_buf.pend(); }
    // Pointer to one past the last byte of reserved memory.
    inline const char_type* outpend(void) const noexcept { return m_buf.pend(); }

    // Mark the next count characters in the stream as being initialized 
    // (i.e. the same as having been written by put() or putn()). A call to this
    // is only required if you use the pointers outpbegin(), outpcur(), or outpend()
    // to modify stream data. If count is larger than the remaining reserved memory, 
    // the behavior is undefined. After this call, outpos() increases by count.
    inline void outcommit(streamsize_type count) noexcept { m_pos += count; }

private:
    internal::buffer<char_type, Allocator> m_buf;
    streamsize_type m_pos;
};

using omemstream = basic_omemstream<std::allocator<sijson::byte>>;

}

#endif