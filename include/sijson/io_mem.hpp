
#ifndef SIJSON_IO_MEM_HPP
#define SIJSON_IO_MEM_HPP

#include <cstddef>
#include <cassert>
#include <cstring>
#include <memory>
#include <algorithm>
#include <stdexcept>

#include "internal/core.hpp"

namespace sijson {

//
// Input memory.
// ------------------------
// Implements ContiguousInput.
// ------------------------
// char_type is unsigned char.
// ------------------------
//
class in_mem
{
public:
    using char_type = unsigned char;
    using size_type = std::size_t;
    using input_kind = io_contiguous;

public:
    in_mem(memspan<const char_type> src) :
        m_beg(src.begin), m_cur(src.begin), m_end(src.end)
    {
#if SIJSON_USE_LOGIC_ERRORS
        if (!src.begin || !src.end)
            throw std::invalid_argument(
                SIJSON_STRFY(sijson::in_mem) + ": src is null.");
#else
        SIJSON_ASSERT(src.begin && src.end);
#endif
    }

    in_mem(const char_type* src, size_type size) :
        in_mem({ src, src + size })
    {}

    in_mem(in_mem&&) = default;
    in_mem(const in_mem&) = delete;

    in_mem& operator=(in_mem&&) = default;
    in_mem& operator=(const in_mem&) = delete;

    // Get byte. If end(), behavior is undefined.
    inline char_type peek(void) const noexcept { return *m_cur; }

    // Extract byte. If end(), behavior is undefined.
    inline char_type take(void) noexcept { return *m_cur++; }

    // True if input has run out of bytes.
    inline bool end(void) const noexcept { return m_cur == m_end; }

    // Jump to the beginning.
    inline void rewind(void) noexcept { m_cur = m_beg; }

    // Get input position.
    inline size_type ipos(void) const noexcept
    {
        return (size_type)(m_cur - m_beg);
    }

    // Pointer to the first byte.
    inline const char_type* ipbeg(void) const noexcept { return m_beg; }

    // Pointer to the current byte.
    inline const char_type* ipcur(void) const noexcept { return m_cur; }

    // Pointer to one past the last byte.
    inline const char_type* ipend(void) const noexcept { return m_end; }

    // Commit the next count bytes (i.e. mark them as 'read'.
    // Required only if you use ipbeg(), ipcur() or ipend() to read data).
    // Increments the input position by count.
    inline void icommit(size_type count) noexcept { m_cur += count; }

private:
    const char_type* m_beg;
    const char_type* m_cur;
    const char_type* m_end;
};


//
// Output memory.
// ------------------------
// Implements ContiguousOutput.
// ------------------------
// Does not optimize for short strings (unlike out_str).
// char_type is unsigned char.
// Supports custom allocators and fancy pointers.
// ------------------------
//
template <typename Allocator = std::allocator<unsigned char>>
class basic_out_mem
{
public:
    using char_type = unsigned char;
    using size_type = std::size_t;
    using allocator_type = Allocator;
    using output_kind = io_contiguous;

public:
    basic_out_mem(size_type capacity,
        const Allocator& alloc = Allocator()
    ) :
        m_pos(0), m_buf{ capacity, alloc }
    {}

    basic_out_mem(const Allocator& alloc = Allocator()) :
        m_pos(0), m_buf{ alloc }
    {}

    basic_out_mem(basic_out_mem&&) = default;
    basic_out_mem(const basic_out_mem& rhs) = default;

    basic_out_mem& operator=(basic_out_mem&&) = default;
    basic_out_mem& operator=(const basic_out_mem&) = default;

    // Put a byte.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put(char_type c)
    {
        m_buf.reserve(m_pos + 1);
        m_buf[m_pos++] = c;
    }

    // Put a byte multiple times.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_f(char_type c, size_type count)
    {
        m_buf.reserve(m_pos + count);
        std::memset(&m_buf[m_pos], c, count);
        m_pos += count;
    }

    // Put bytes from an array.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void put_n(const char_type* arr, std::size_t count)
    {
        m_buf.reserve(m_pos + count);
        std::memcpy(&m_buf[m_pos], arr, count);
        m_pos += count;
    }

    // No effect.
    inline void flush(void) noexcept {}

    // Get output position.
    inline size_type opos(void) const noexcept { return m_pos; }

    // Increase # reserved bytes to at least new_capacity.
    // If this function fails for any reason, it 
    // has no effect (strong exception guarantee).
    inline void reserve(size_type new_capacity)
    {
        m_buf.reserve(new_capacity);
    }
 
    // Pointer to the first byte.
    inline char_type* opbeg(void) noexcept { return m_buf.pbegin(); }

    // Pointer to the first byte.
    inline const char_type* opbeg(void) const noexcept { return m_buf.pbegin(); }

    // Pointer to the next writable byte.
    inline char_type* opcur(void) noexcept { return m_buf.pbegin() + m_pos; }

    // Pointer to the next writable byte.
    inline const char_type* opcur(void) const noexcept { return m_buf.pbegin() + m_pos; }

    // Pointer to one past the last byte of reserved memory.
    inline char_type* opend(void) noexcept { return m_buf.pend(); }

    // Pointer to one past the last byte of reserved memory.
    inline const char_type* opend(void) const noexcept { return m_buf.pend(); } 

    // Commit the next count bytes (i.e. mark them as 'written to'.
    // Required only if you use opbeg(), opcur() or opend() to write data).
    // Increments the output position by count.
    inline void ocommit(size_type count) noexcept { m_pos += count; }

private:
    size_type m_pos;
    iutil::buffer<char_type, Allocator> m_buf;
};

using out_mem = basic_out_mem<std::allocator<unsigned char>>;

}

#endif