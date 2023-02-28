
#ifndef SIJSON_FILESTREAM
#define SIJSON_FILESTREAM

#include <cstddef>
#include <cstdio>
#include <cstring>
#include <limits>
#include <memory>
#include <string>
#include <utility>
#include <algorithm>
#include <stdexcept>

#include "core.hpp"
#include "internal/util.hpp"
#include "internal/file.hpp"
#include "internal/buffers.hpp"

namespace sijson {

// Input file stream.
class ifilestream
{
public:
    using char_type = char;
    using size_type = std::size_t;
    using input_kind = tag::io_buffered;

private:
    ifilestream(internal::file&& file, std::size_t bufsize) :
        m_file(std::move(file)),
        m_buf{ iutil::uround_up(bufsize, internal::file::SYS_BUFSIZE) },
        m_buf_cur(m_buf.pbegin()),
        m_buf_last(m_buf.pbegin()),
        m_buf_eof(false),
        m_posn(0)
    {
#if SIJSON_LOGIC_ERRORS
        if (bufsize == 0)
            throw std::invalid_argument(std::string(__func__) + ": Buffer size is 0.");
#else
        assert(bufsize > 0);
#endif
        if (!m_file.is_open())
            throw std::runtime_error("Could not open file.");

        // set EOF
        refill_buf(); 
    }

public:
    ifilestream(const char filepath[],
        std::size_t bufsize = internal::file::DEFAULT_BUFSIZE
    ) :
        ifilestream({ filepath, "rb" }, bufsize)
    {}

#ifdef _MSC_VER
    ifilestream(const wchar_t filepath[],
        std::size_t bufsize = internal::file::DEFAULT_BUFSIZE
    ) :
        ifilestream({ filepath, L"rb" }, bufsize)
    {}
#endif

    ifilestream(ifilestream&&) = default;
    ifilestream(const ifilestream&) = delete;

    ifilestream& operator=(ifilestream&&) = default;
    ifilestream& operator=(const ifilestream&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char peek(void) const noexcept { return *m_buf_cur; }

    // Extract character. If end(), behavior is undefined.
    SIJSON_ALWAYS_INLINE char take(void)
    {
        char c = *m_buf_cur;

        if (m_buf_cur < m_buf_last) {
            m_buf_cur++; m_posn++;
        }
        else {
            if (!m_buf_eof) m_posn++;
            refill_buf();
        }
        return c;
    }

    // Get position.
    inline size_type ipos(void) const noexcept { return m_posn; }

    // True if stream has run out of characters.
    inline bool end(void) const noexcept
    {
        return m_buf_eof && m_buf_cur == m_buf_last;
    }

    // Jump to the beginning of the stream.
    inline void rewind(void)
    {
        if (m_posn == buf_used())
            m_buf_cur = m_buf.pbegin();
        else 
        {   // refilled buffer before
            m_file.rewind();
            reset_buf();
            refill_buf();
        }
        m_posn = 0;       
    }

    //inline const char_type* ibufbeg()

    // Close file. Throws on failure.
    // Whether or not the operation succeeds,
    // the stream will no longer be usable.
    inline void close(void) 
    {
        if (!m_file.close())
            throw std::runtime_error("Could not close file.");
    }

private:
    inline std::size_t buf_avail(void) const noexcept { return (std::size_t)(m_buf_last + 1 - m_buf_cur); }
    inline std::size_t buf_used(void) const noexcept { return (std::size_t)(m_buf_cur - m_buf.pbegin()); }

    // Throws on I/O failure.
    // Sets buf_eof if further refills are unnecessary.
    // Returns num bytes read.
    inline std::size_t refill_buf(void)
    {
        if (m_buf_eof) return 0;

        std::size_t nread = m_file.read(m_buf.pbegin(), m_buf.capacity());
        if (m_file.error())
            throw std::runtime_error("Could not read from file.");

        m_buf_last = nread > 0 ? m_buf.pbegin() + nread - 1 : m_buf.pbegin();
        m_buf_cur = m_buf.pbegin();

        if (nread < m_buf.capacity())
        {
            // ensure last byte is read
            if (nread > 0) m_buf_last++;
            m_buf_eof = true;
        }

        return nread;
    }

    inline void reset_buf(void)
    {
        m_buf_cur = m_buf.pbegin();
        m_buf_last = m_buf.pbegin();
        m_buf_eof = false;
    }

private:
    internal::file m_file;
    internal::buffer<char> m_buf;
    char* m_buf_cur;
    char* m_buf_last; // always >= m_buf_cur
    bool m_buf_eof;
    size_type m_posn;
};


// Output file stream.
class ofilestream
{
public:
    using char_type = char;
    using size_type = std::size_t;
    using output_kind = tag::io_buffered;

private:
    ofilestream(internal::file&& file, std::size_t bufsize) :
        m_file(std::move(file)),
        m_buf{ iutil::uround_up(bufsize, internal::file::SYS_BUFSIZE) },        
        m_buf_cur(m_buf.pbegin()),
        m_posn(0)
    {
#if SIJSON_LOGIC_ERRORS
        if (bufsize == 0)
            throw std::invalid_argument(std::string(__func__) + ": Buffer size is 0.");
#else
        assert(bufsize > 0);
#endif
        if (!m_file.is_open())
            throw std::runtime_error("Could not open file.");
    }

public:
    ofilestream(const char filepath[],
        std::size_t bufsize = internal::file::DEFAULT_BUFSIZE
    ) :
        ofilestream({ filepath, "wb" }, bufsize)
    {}

#ifdef _MSC_VER
    ofilestream(const wchar_t filepath[],
        std::size_t bufsize = internal::file::DEFAULT_BUFSIZE
    ) :
        ofilestream({ filepath, L"wb" }, bufsize)
    {}
#endif

    ofilestream(ofilestream&&) = default;
    ofilestream(const ofilestream&) = delete;

    ofilestream& operator=(ofilestream&&) = default;
    ofilestream& operator=(const ofilestream&) = delete;

    // Put a character.
    SIJSON_ALWAYS_INLINE void put(char c)
    {
        if (buf_avail() == 0)
            flush_buf();

        *m_buf_cur++ = c; 
        m_posn++;
    }

    // Put the same character multiple times.
    SIJSON_ALWAYS_INLINE void put(char c, std::size_t count)
    {
        if (count <= buf_avail())
        {
            std::memset(m_buf_cur, c, count);
            m_buf_cur += count;
            m_posn += count;
        }
        else fill_write(c, count);
    }

    // Put characters from an array.
    SIJSON_ALWAYS_INLINE void put_n(const char* str, std::size_t count)
    {
        if (count <= buf_avail())
        {
            std::memcpy(m_buf_cur, str, count);
            m_buf_cur += count;
            m_posn += count;
        }
        else bulk_write(str, count);
    }

    // Synchronize with target.
    inline void flush(void)
    {
        if (!flush_buf<false>() || !m_file.flush())
            throw std::runtime_error("Could not flush file buffer.");
    }

    // Get position.
    inline size_type opos(void) const noexcept { return m_posn; }

    // Flush stream and close file. Throws on failure.
    // Whether or not the operation succeeds,
    // the stream will no longer be usable.
    inline void close(void)
    {
        // always close, even if flush fails
        bool flushed = flush_buf<false>();
        bool closed = m_file.close();

        if (!flushed || !closed)
            throw std::runtime_error("Could not close file.");
    }

    ~ofilestream(void) noexcept
    {
        flush_buf<false>(); // ignore errors
        // file flushes + closes in destructor
    }

private:
    inline std::size_t buf_avail(void) const noexcept { return (std::size_t)(m_buf.pend() - m_buf_cur); }
    inline std::size_t buf_pending(void) const noexcept { return (std::size_t)(m_buf_cur - m_buf.pbegin()); }
    
    inline std::runtime_error fwrite_error(void)
    {
        return std::runtime_error("Could not write to file.");
    }

    template <bool ThrowOnError = true>
    inline bool flush_buf(void)
    {       
        auto npending = buf_pending();

        auto nwrote = m_file.write(m_buf.pbegin(), npending);
        if (nwrote != npending)
        {
            if (ThrowOnError)
                throw fwrite_error();
            else return false;
        }

        m_buf_cur = m_buf.pbegin();
        return true;
    }

    // pos() is unchanged until all bytes are written.
    inline void fill_write(char c, std::size_t count)
    {
        SIJSON_ASSERT(count > buf_avail());

        auto rem = count;
        while (rem > 0)
        {
            if (buf_avail() == 0)
                flush_buf();

            // put n chars each time
            auto n = std::min(rem, buf_avail());
            std::memset(m_buf_cur, c, n);
            m_buf_cur += n;
            rem -= n;
        }

        m_posn += count;
    }

    // pos() is unchanged until all bytes are written.
    inline void bulk_write(const char* src, std::size_t count)
    {       
        SIJSON_ASSERT(count > buf_avail());

        auto rem = count;

        // fill buffer first
        auto nbuf = buf_avail();
        std::memcpy(m_buf_cur, src, nbuf);
        m_buf_cur += nbuf;
        src += nbuf; rem -= nbuf;

        flush_buf();

        // write (multiple of buffer cap) bytes directly
        auto nfile = rem - rem % m_buf.capacity();
        if (nfile != 0)
        {
            auto nwrote = m_file.write(src, nfile);
            if (nwrote != nfile)
                throw fwrite_error();
            src += nfile; rem -= nfile;
        }

        // if any remains, buffer it
        if (rem != 0)
        {
            std::memcpy(m_buf_cur, src, rem);
            m_buf_cur += rem;
        }
        
        m_posn += count;
    }

private:
    internal::file m_file;
    internal::buffer<char> m_buf;
    char* m_buf_cur;
    size_type m_posn;
};

}
#endif