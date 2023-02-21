
#ifndef SIJSON_INTERNAL_FILE_HPP
#define SIJSON_INTERNAL_FILE_HPP

#include <cstddef>
#include <cstdio>

#include "config.hpp"
#include "util.hpp"


namespace sijson {
namespace internal {

class file
{
public:
    static constexpr std::size_t SYS_BUFSIZE = BUFSIZ;

    // A better default than SYS_BUFSIZE.
    static constexpr std::size_t DEFAULT_BUFSIZE =
        iutil::uround_up(static_cast<std::size_t>(SIJSON_FILEBUF_SIZE), SYS_BUFSIZE);

public:
    file(const char filepath[], const char mode[]) :
        m_fptr(file::open(filepath, mode))
    {
        if (is_open())
            set_std_unbuffered();
    }

#ifdef _MSC_VER
    file(const wchar_t filepath[], const wchar_t mode[]) :
        m_fptr(file::wopen(filepath, mode))
    {
        if (is_open())
            set_std_unbuffered();
    }
#endif

    file(file&& rhs) noexcept : 
        m_fptr(rhs.m_fptr)
    {
        rhs.m_fptr = nullptr;
    }

    file(const file&) = delete;
    file& operator=(const file&) = delete;

    file& operator=(file&& rhs) noexcept
    {
        if (this != &rhs)
        {
            m_fptr = rhs.m_fptr;
            rhs.m_fptr = nullptr;
        }
        return *this;
    }

    // Returns true if file is ready for use.
    inline bool is_open(void) const noexcept { return m_fptr != nullptr; }   

    template <typename T>
    inline std::size_t read(T* buffer, std::size_t count) noexcept
    {
#ifdef _MSC_VER
        return ::fread_s(buffer, count, sizeof(T), count, m_fptr);
#else    
        return std::fread(buffer, sizeof(T), count, m_fptr);
#endif
    }

    template <typename T>
    inline std::size_t write(const T* buffer, std::size_t count) noexcept
    {
        return std::fwrite(buffer, sizeof(T), count, m_fptr);
    }

    inline bool flush() noexcept { return std::fflush(m_fptr) == 0; }

    inline int error(void) noexcept { return std::ferror(m_fptr); }

    inline void rewind(void) noexcept { std::rewind(m_fptr); }

    // Returns true on success.
    // Whether or not the operation succeeds,
    // the file will no longer be usable.
    inline bool close(void) noexcept
    {
        if (m_fptr)
        {
            int ret = std::fclose(m_fptr);
            m_fptr = nullptr;
            return ret == 0;
        }
        else return true;
    }

    // Ignore errors on close.
    ~file(void) noexcept { if (m_fptr) std::fclose(m_fptr); }

private:
    inline bool set_std_unbuffered(void) noexcept
    {
        // setbuf() is deprecated in MSVC
        return std::setvbuf(m_fptr, nullptr, _IONBF, 0) == 0;
    }

    static inline std::FILE* open(const char filepath[], const char mode[])
    {
#ifdef _MSC_VER
        std::FILE* file;
        if (::fopen_s(&file, filepath, mode) != 0)
            file = nullptr;
#else
        assert(filepath && mode);
        std::FILE* file = std::fopen(filepath, mode);
#endif
        return file;
    }

#ifdef _MSC_VER
    static inline std::FILE* wopen(const wchar_t filepath[], const wchar_t mode[])
    {
        std::FILE* file;
        if (::_wfopen_s(&file, filepath, mode) != 0)
            file = nullptr;
        return file;
    }
#endif

private:
    std::FILE* m_fptr;
};
}

}

#endif