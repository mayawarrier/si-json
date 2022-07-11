
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

#include "common.hpp"
#include "internal/util.hpp"
#include "internal/impl_file.hpp"
#include "internal/buffers.hpp"

namespace sijson {

// Input file stream.
class ifilestream
{
private:
    ifilestream(internal::file&& file, std::size_t bufsize) :
        m_file(std::move(file)),
        m_buf{ iutil::uround_up(bufsize, internal::file::SYS_BUFSIZE) },
        m_buf_cur(m_buf.begin()),
        m_buf_last(m_buf.begin()),
        m_buf_eof(false),
        m_posn(0)
    {
        if (bufsize == 0)
            throw std::invalid_argument("Buffer size is 0");

        if (!m_file.is_open())
            throw std::runtime_error(std::string("Could not open ") + m_file.path());

        // disable std buffer
        m_file.set_unbuffered();

        // set EOF
        refill_buf(); 
    }

public:
    ifilestream(const char filepath[],
        std::size_t bufsize = internal::file::SYS_BUFSIZE
    ) :
        ifilestream({ filepath, "rb" }, bufsize)
    {}

#ifdef _MSC_VER
    ifilestream(const wchar_t filepath[],
        std::size_t bufsize = internal::file::SYS_BUFSIZE
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
    inline char take(void)
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
    inline std::size_t inpos(void) const noexcept { return m_posn; }

    // True if the last operation completed by reaching the end of the stream.
    inline bool end(void) const noexcept
    {
        return m_buf_eof && m_buf_cur == m_buf_last;
    }

    // Jump to the beginning of the stream.
    inline void rewind(void)
    {
        if (m_posn == buf_used())
            m_buf_cur = m_buf.begin();
        else 
        {   // refilled buffer before
            m_file.rewind();
            reset_buf();
            refill_buf();
        }
        m_posn = 0;       
    }

    // Close file. Throws on failure.
    // Whether or not the operation succeeds,
    // the stream will no longer be usable.
    inline void close(void) 
    {
        if (!m_file.close())
            throw std::runtime_error(std::string("Could not close ") + m_file.path());
    }

private:
    inline std::size_t buf_avail(void) const noexcept { return (std::size_t)(m_buf_last + 1 - m_buf_cur); }
    inline std::size_t buf_used(void) const noexcept { return (std::size_t)(m_buf_cur - m_buf.begin()); }

    // Throws on I/O failure.
    // Sets buf_eof if further refills are unnecessary.
    // Returns num bytes read.
    inline std::size_t refill_buf(void)
    {
        if (m_buf_eof) return 0;

        std::size_t nread = m_file.read(m_buf.begin(), m_buf.capacity());
        if (m_file.error())
            throw std::runtime_error(std::string("Could not read from ") + m_file.path());

        m_buf_last = nread > 0 ? m_buf.begin() + nread - 1 : m_buf.begin();
        m_buf_cur = m_buf.begin();

        if (nread < m_buf.capacity()) 
            m_buf_eof = true;

        return nread;
    }

    inline void reset_buf(void)
    {
        m_buf_cur = m_buf.begin();
        m_buf_last = m_buf.begin();
        m_buf_eof = false;
    }

private:
    internal::file m_file;
    internal::buffer<char> m_buf;
    char* m_buf_cur;
    char* m_buf_last; // always >= m_buf_cur
    bool m_buf_eof;
    std::size_t m_posn;
};


// Output file stream.
class ofilestream
{
private:
    ofilestream(internal::file&& file, std::size_t bufsize) :
        m_file(std::move(file)),
        m_buf{ iutil::uround_up(bufsize, internal::file::SYS_BUFSIZE) },        
        m_buf_cur(m_buf.begin()),
        m_posn(0)
    {
        if (bufsize == 0)
            throw std::invalid_argument("Buffer size is 0");

        if (!m_file.is_open())
            throw std::runtime_error(std::string("Could not open ") + m_file.path());

        // disable std buffer
        m_file.set_unbuffered();
    }

public:
    ofilestream(const char filepath[],
        std::size_t bufsize = internal::file::SYS_BUFSIZE
    ) :
        ofilestream({ filepath, "wb" }, bufsize)
    {}

#ifdef _MSC_VER
    ofilestream(const wchar_t filepath[],
        std::size_t bufsize = internal::file::SYS_BUFSIZE
    ) :
        ofilestream({ filepath, L"wb" }, bufsize)
    {}
#endif

    ofilestream(ofilestream&&) = default;
    ofilestream(const ofilestream&) = delete;

    ofilestream& operator=(ofilestream&&) = default;
    ofilestream& operator=(const ofilestream&) = delete;

    // Put a character.
    inline void put(char c)
    {
        if (buf_avail() == 0)
            flush_buf();

        *m_buf_cur++ = c; 
        m_posn++;
    }

    // Put the same character multiple times.
    inline void put(char c, std::size_t count)
    {
        if (count <= buf_avail())
        {
            std::memset(m_buf_cur, c, count);
            m_buf_cur += count;
            m_posn += count;
        }
        else buffered_write(count, [&](std::size_t n) {
            std::memset(m_buf_cur, c, n);
        });
    }

    // Put characters from an array.
    inline void putn(const char* str, std::size_t count)
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
            throw std::runtime_error(std::string("Could not sync with ") + m_file.path());
    }

    // Get position.
    inline std::size_t outpos(void) const noexcept { return m_posn; }

    // Flush stream and close file. Throws on failure.
    // Whether or not the operation succeeds,
    // the stream will no longer be usable.
    inline void close(void)
    {
        // always close, even if flush fails
        bool flushed = flush_buf<false>();
        bool closed = m_file.close();

        if (!flushed || !closed)
            throw std::runtime_error(std::string("Could not close ") + m_file.path());
    }

    ~ofilestream(void) noexcept
    {
        flush_buf<false>(); // ignore errors
        // file flushes + closes in destructor
    }

private:
    inline std::size_t buf_avail(void) const noexcept { return (std::size_t)(m_buf.end() - m_buf_cur); }
    inline std::size_t buf_pending(void) const noexcept { return (std::size_t)(m_buf_cur - m_buf.begin()); }
    
    inline std::runtime_error fwrite_error(void)
    {
        return std::runtime_error(std::string("Could not write to ") + m_file.path());
    }

    template <bool throw_on_error = true>
    inline bool flush_buf(void)
    {       
        auto npending = buf_pending();

        auto nwrote = m_file.write(m_buf.begin(), npending);
        if (nwrote != npending)
        {
            if (throw_on_error)
                throw fwrite_error();
            else return false;
        }

        m_buf_cur = m_buf.begin();
        return true;
    }

    // pos() is unchanged until all bytes are written.
    template <typename Func>
    inline void buffered_write(std::size_t count, Func write_some)
    {
        auto rem = count;
        assert(rem > buf_avail());

        while (rem > 0)
        {
            if (buf_avail() == 0)
                flush_buf();

            // put n chars each time
            auto n = std::min(rem, buf_avail());
            write_some(n);
            m_buf_cur += n;
            rem -= n;
        }

        m_posn += count;
    }

    // pos() is unchanged until all bytes are written.
    inline void bulk_write(const char* src, std::size_t count)
    {
        auto rem = count;
        assert(rem > buf_avail());

        // fill buffer first
        auto nbuf = buf_avail();
        std::memcpy(m_buf_cur, src, nbuf);
        m_buf_cur += nbuf;
        src += nbuf; rem -= nbuf;

        flush_buf();

        // write largest multiple of buffer size directly
        auto nfile = rem - rem % m_buf.capacity();
        if (nfile != 0)
        {
            auto nwrote = m_file.write(src, nfile);
            if (nwrote != nfile)
                throw fwrite_error();
            src += nfile; rem -= nfile;
        }

        // if any remains, buffer it
        // (this is always less than capacity)
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
    std::size_t m_posn;
};

}
#endif