
#ifndef SIJSON_INTERNAL_IMPL_FILE_HPP
#define SIJSON_INTERNAL_IMPL_FILE_HPP

#include <cstddef>
#include <cstdio>
#include <limits>
#include <ios>
#include <string>
#include <sstream>
#include <utility>
#include <type_traits>
#include <stdexcept>

// convert wide path to utf8
#if defined(_MSC_VER) && _MSVC_LANG < 201703L
#include <locale>
#include <codecvt>
#endif


namespace sijson {
namespace internal {

class file
{
public:
    static constexpr std::size_t SYS_BUFSIZE = BUFSIZ;

    file(const char filepath[], const char mode[]) :
        m_fpath(filepath),
        m_fptr(file::open(filepath, mode))
    {}

#ifdef _MSC_VER
    file(const wchar_t filepath[], const wchar_t mode[]) :
        m_fpath(file::to_utf8path(filepath)),
        m_fptr(file::wopen(filepath, mode))
    {}
#endif

    file(file&& rhs) noexcept : 
        m_fpath(std::move(rhs.m_fpath)),
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
            m_fpath = std::move(rhs.m_fpath);
            m_fptr = rhs.m_fptr;
            rhs.m_fptr = nullptr;
        }
        return *this;
    }

    // Returns true if file is ready for use.
    inline bool is_open(void) const noexcept { return m_fptr != nullptr; }

    // Can be called only once, before any other operations on the file.
    // The file must be open. Returns true on success.
    inline bool set_unbuffered(void) noexcept 
    {
        // same as setbuf(), but setbuf() is deprecated in MSVC
        return std::setvbuf(m_fptr, nullptr, _IONBF, 0) == 0;
    }

    // File path encoded as UTF-8.
    inline std::string path(void) const noexcept { return m_fpath; }

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

    static inline std::string to_utf8path(const wchar_t filepath[])
    {
#if _MSVC_LANG < 201703L
        std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> conv;
        return conv.to_bytes(filepath);
#else
        // todo when std::path support is added
        // path string is used only for diagnostics at the moment
        return "<wide filepath>";
#endif
    }
#endif

private:
    std::string m_fpath;
    std::FILE* m_fptr;
};
}

}

#endif