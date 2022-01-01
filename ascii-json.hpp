
#ifndef ASCII_JSON_HPP
#define ASCII_JSON_HPP

#include <cstddef>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdint>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <utility>
#include <algorithm>
#include <memory>
#include <string>
#include <iostream>
#include <sstream>
#include <locale>
#include <vector>
#include <stack>
#include <stdexcept>
#include <type_traits>

namespace json {

using intl32_t = std::int_least32_t;
using intl64_t = std::int_least64_t;
using uintl32_t = std::uint_least32_t;
using uintl64_t = std::uint_least64_t;

namespace internal
{
template <bool test, typename T = int>
using require_t = typename std::enable_if<test, T>::type;

template <typename A, typename B>
using require_same_t = require_t<std::is_same<A, B>::value>;

static constexpr auto int32_max = 0x7FFFFFFF;
static constexpr auto int32_min = -0x7FFFFFFF - 1;
static constexpr auto uint32_max = 0xFFFFFFFF;

static constexpr auto int64_max = 0x7FFFFFFFFFFFFFFF;
static constexpr auto int64_min = -0x7FFFFFFFFFFFFFFF - 1;
static constexpr auto uint64_max = 0xFFFFFFFFFFFFFFFF;

template <typename T> struct least_t_exp_digits {};
template <> struct least_t_exp_digits<intl32_t>  : std::integral_constant<int, 31> {};
template <> struct least_t_exp_digits<intl64_t>  : std::integral_constant<int, 63> {};
template <> struct least_t_exp_digits<uintl32_t> : std::integral_constant<int, 32> {};
template <> struct least_t_exp_digits<uintl64_t> : std::integral_constant<int, 64> {};

template <typename T> struct least_t_exp_min {};
template <> struct least_t_exp_min<intl32_t>  : std::integral_constant<intl32_t, int32_min> {};
template <> struct least_t_exp_min<intl64_t>  : std::integral_constant<intl64_t, int64_min> {};
template <> struct least_t_exp_min<uintl32_t> : std::integral_constant<uintl32_t, 0> {};
template <> struct least_t_exp_min<uintl64_t> : std::integral_constant<uintl64_t, 0> {};

template <typename T> struct least_t_exp_max {};
template <> struct least_t_exp_max<intl32_t>  : std::integral_constant<intl32_t, int32_max> {};
template <> struct least_t_exp_max<intl64_t>  : std::integral_constant<intl64_t, int64_max> {};
template <> struct least_t_exp_max<uintl32_t> : std::integral_constant<uintl32_t, uint32_max> {};
template <> struct least_t_exp_max<uintl64_t> : std::integral_constant<uintl64_t, uint64_max> {};

template <typename T>
struct is_nonbool_integral : std::integral_constant<bool,
    std::is_integral<T>::value && !std::is_same<T, bool>::value>
{};

template <typename T>
struct is_nb_signed_integral : std::integral_constant<bool,
    is_nonbool_integral<T>::value && std::is_signed<T>::value>
{};
template <typename T>
struct is_nb_unsigned_integral : std::integral_constant<bool,
    is_nonbool_integral<T>::value && std::is_unsigned<T>::value>
{};

#if _MSVC_LANG >= 201703L || __cplusplus >= 201703L
template <typename ...Bn>
using conjunction = std::conjunction<Bn...>;
#else
// https://en.cppreference.com/w/cpp/types/conjunction
template <typename...> struct conjunction : std::true_type { };
template <typename B1> struct conjunction<B1> : B1 { };
template <typename B1, typename ...Bn>
struct conjunction<B1, Bn...> : std::conditional<bool(B1::value), conjunction<Bn...>, B1>::type {};
#endif
}

namespace internal
{
class reflect
{
public:
    template <typename T>
    static auto is_cistream(int) -> decltype(
        require_same_t<decltype(std::declval<T>().peek()), char>{},
        require_same_t<decltype(std::declval<T>().take()), char>{},
        require_same_t<decltype(std::declval<T>().pos()), std::size_t>{},
        require_same_t<decltype(std::declval<T>().end()), bool>{}, std::true_type{})
    { return {}; }

    template <typename T>
    static auto is_costream(int) -> decltype(
        std::declval<T>().put((char)0), std::declval<T>().flush(), 
        require_same_t<decltype(std::declval<T>().pos()), std::size_t>{}, std::true_type{})
    { return {}; }

    template <typename T>
    static auto is_rewindable(int) -> decltype(std::declval<T>().rewind(), std::true_type{})
    { return {}; }

    template <typename>
    static auto is_cistream(...)->std::false_type { return {}; }
    template <typename>
    static auto is_costream(...)->std::false_type { return {}; }
    template <typename>
    static auto is_rewindable(...)->std::false_type { return {}; }
};
}

// is_cistream<T>::value is true if T is
// a character input stream i.e. it implements:
// - char peek(); Get character. if end(), returns nulchar.
// - char take(); Extract character. if end(), returns nulchar.
// - std::size_t pos(); Get position. Must be equal to # of calls to take().
// - bool end(); True if there are no more chars to extract.
//
template <typename T>
struct is_cistream : decltype(internal::reflect::is_cistream<T>(0)) {};

// is_costream<T>::value is true if T is
// a character output stream i.e. it implements:
// - put(char); Push a character.
// - flush(); Push any buffered chars to target.
// - std::size_t pos(); Get position. Must be equal to # of calls to put().
//
template <typename T>
struct is_costream : decltype(internal::reflect::is_costream<T>(0)) {};

// is_rewindable<T>::value is true if T implements:
// - rewind(); Jump to the beginning of the stream.
template <typename T>
struct is_rewindable : decltype(internal::reflect::is_rewindable<T>(0)) {};


// Interfaces may be used in non-templated 
// contexts or to reduce binary size.
namespace interfaces
{
// Interface to a character input stream.
class cistream
{
private:
    void* impl;
    char(*peek_impl)(void*);
    char(*take_impl)(void*);
    std::size_t(*pos_impl)(void*);
    bool(*end_impl)(void*);

public:
    template <typename T, typename = internal::require_t<is_cistream<T>::value>>
    cistream(T& stream) noexcept :
        impl(&stream),
        peek_impl([](void* is) -> char { return ((T*)is)->peek(); }),
        take_impl([](void* is) -> char { return ((T*)is)->take(); }),
        pos_impl([](void* is) -> std::size_t { return ((T*)is)->pos(); }),
        end_impl([](void* is) -> bool { return ((T*)is)->end(); })
    {}
    cistream(cistream&&) = default;
    cistream(const cistream&) = default;

    cistream& operator=(cistream&&) = default;
    cistream& operator=(const cistream&) = default;

    // Get character. if end(), returns nulchar.
    inline char peek(void) const { return peek_impl(impl); }
    // Extract character. if end(), returns nulchar.
    inline char take(void) const { return take_impl(impl); }
    // Get position.
    inline std::size_t pos(void) const { return pos_impl(impl); }
    // True if there are no more chars to extract.
    inline bool end(void) const { return end_impl(impl); }

    virtual ~cistream(void) {}
};

// Interface to a rewindable stream.
class rewindable
{
private:
    void* impl;
    void(*rewind_impl)(void*);

public:
    template <typename T, typename = internal::require_t<is_rewindable<T>::value>>
    rewindable(T& rew) noexcept :
        impl(&rew),
        rewind_impl([](void* is) { ((T*)is)->rewind(); })
    {}
    rewindable(rewindable&&) = default;
    rewindable(const rewindable&) = default;

    rewindable& operator=(rewindable&&) = default;
    rewindable& operator=(const rewindable&) = default;

    // Jump to the beginning of the stream.
    inline void rewind(void) const { rewind_impl(impl); }

    virtual ~rewindable(void) {}
};

// Interface to a character output stream.
class costream
{
private:
    void* impl;
    void(*flush_impl)(void*);
    void(*put_impl)(void*, char);
    std::size_t(*pos_impl)(void*);

public:
    template <typename T, typename = internal::require_t<is_costream<T>::value>>
    costream(T& stream) noexcept :
        impl(&stream),
        flush_impl([](void* os) { ((T*)os)->flush(); }),
        put_impl([](void* os, char c) { ((T*)os)->put(c); }),
        pos_impl([](void* os) -> std::size_t { return ((T*)os)->pos(); })
    {}
    costream(costream&&) = default;
    costream(const costream&) = default;

    costream& operator=(costream&&) = default;
    costream& operator=(const costream&) = default;

    // Push a character. 
    inline void put(char c) const { put_impl(impl, c); }
    // Push any buffered chars to target.
    inline void flush(void) const { flush_impl(impl); }
    // Get position.
    inline std::size_t pos(void) const { return pos_impl(impl); }

    virtual ~costream(void) {}
};

// Interface to a type that implements one or more interfaces.
// An instance of many<> can be cast to any one of the interfaces
// implemented by the type.
template <typename ...Ifaces>
class many : public Ifaces...
{
public:
    static_assert(internal::conjunction<
        std::has_virtual_destructor<Ifaces>...>::value,
        "Requires all component interfaces to have virtual destructors.");

    template <typename Timpl>
    many(Timpl& impl) : Ifaces(impl)...
    {}
    many(many&&) = default;
    many(const many&) = default;

    many& operator=(many&&) = default;
    many& operator=(const many&) = default;
    
    virtual ~many(void) {}
};
}

namespace internal
{
class fstream_base
{
protected:
    using unique_file_ptr = std::unique_ptr<std::FILE, void(*)(std::FILE*)>;

    static inline unique_file_ptr make_unique_file_ptr(const char* filepath, const char* mode)
    {
#ifdef _MSC_VER
        std::FILE* file;
        bool err = ::fopen_s(&file, filepath, mode) != 0;
#else
        assert(filepath && mode);
        std::FILE* file = std::fopen(filepath, mode);
        bool err = !file;
#endif
        if (err) throw std::runtime_error(std::string("Could not open ") + filepath);
        return { file, [](std::FILE* fp) { std::fclose(fp); } };
    }

    static inline std::size_t fread(void* buffer, std::size_t bufsize,
        std::size_t elemsize, std::size_t count, std::FILE* stream) noexcept
    {
#ifdef _MSC_VER
        return ::fread_s(buffer, bufsize, elemsize, count, stream);
#else
        (void)bufsize;
        assert(buffer && stream && bufsize >= count * elemsize);
        return std::fread(buffer, elemsize, count, stream);
#endif
    }
};
}

// Input file stream.
class ifstream : internal::fstream_base
{
private:
    const char* filepath;
    unique_file_ptr file;
    std::size_t bufsize;
    std::unique_ptr<char[]> buf;
    char* buf_last_read;
    bool buf_eof;
    char* current;
    std::size_t posn;
    
public:
    ifstream(const char filepath[], std::size_t bufsize = 256) :
        filepath(filepath), bufsize(bufsize),
        file(make_unique_file_ptr(filepath, "rb")),
        buf(new char[bufsize]), buf_last_read(buf.get()), buf_eof(false),
        current(buf.get()), posn(SIZE_MAX)
    {
        if (bufsize == 0)
            throw std::invalid_argument("Buffer size is 0");
        // fill buffer
        advance();
    }

    ifstream(ifstream&&) = default;
    ifstream(const ifstream&) = delete;
    
    ifstream& operator=(ifstream&&) = default;
    ifstream& operator=(const ifstream&) = delete;

    // Get character. If end(), returns nulchar.
    inline char peek(void) const noexcept { return *current; }

    // Extract character. if end(), returns nulchar.
    inline char take(void) {
        char c = *current;
        advance();
        return c;
    }

    // Get position.
    inline std::size_t pos(void) const noexcept { return posn; }

    // True if there are no more chars to extract.
    inline bool end(void) const noexcept 
    { return buf_eof && current == buf_last_read; }

    // Close file.
    // Further usage of the stream is undefined behavior.
    inline void close(void) noexcept { file.reset(); }

    // Jump to the beginning of the stream.
    inline void rewind(void)
    {
        if (posn != current - buf.get())
        {
            std::rewind(file.get());

            current = buf.get();
            buf_last_read = buf.get();
            buf_eof = false;
            posn = SIZE_MAX;

            advance(); // fill buffer           
        }
        // buffer was never refilled
        else { current = buf.get(); posn = 0; }
    }

    virtual ~ifstream(void) {}

private:
    // Move ahead (may fill buffer).
    inline void advance(void)
    {
        if (current < buf_last_read) {
            current++; posn++; // remaining
        }
        else if (!buf_eof)
        {
            // fill buffer 
            std::size_t nread = fstream_base::fread(buf.get(), 
                bufsize, sizeof(char), bufsize, file.get());
            if (std::ferror(file.get()))
                throw std::runtime_error(std::string("Failed to read from ") + filepath);

            buf_last_read = buf.get() + nread - 1;
            current = buf.get(); posn++;

            if (nread < bufsize) {
                // end of file
                buf[nread] = 0;
                buf_last_read++;
                buf_eof = true;
            }
        }
    }
};

// Input memory stream.
class imstream
{
private:
    const char* begin;
    const char* current;
    const char* pend;

public:
    imstream(const char* src, std::size_t size) :
        begin(src), current(src), pend(src + size)
    {
        if (!src) throw std::invalid_argument("String is null");
    }
    imstream(imstream&&) = default;
    imstream(const imstream&) = delete;

    imstream& operator=(imstream&&) = default;
    imstream& operator=(const imstream&) = delete;

    // Get character. if end(), returns nulchar.
    inline char peek(void) const noexcept { return end() ? 0 : *current; }
    // Extract character. if end(), returns nulchar.
    inline char take(void) noexcept { return end() ? 0 : *current++; }
    // Get position.
    inline std::size_t pos(void) const noexcept { return (std::size_t)(current - begin); }
    // True if there are no more chars to extract.
    inline bool end(void) const noexcept { return current == pend; }
    // Jump to the beginning of the stream.
    inline void rewind(void) noexcept { current = begin; }

    virtual ~imstream(void) {}
};

// Input C-string stream (null-terminated).
class icsstream
{
private:
    const char* begin;
    const char* current;

public:
    icsstream(const char* src) :
        begin(src), current(src)
    {
        if (!src) throw std::invalid_argument("String is null");
    }
    icsstream(icsstream&&) = default;
    icsstream(const icsstream&) = delete;

    icsstream& operator=(icsstream&&) = default;
    icsstream& operator=(const icsstream&) = delete;

    // Get character. if end(), returns nulchar.
    inline char peek(void) const noexcept { return end() ? 0 : *current; }
    // Extract character. if end(), returns nulchar.
    inline char take(void) noexcept { return end() ? 0 : *current++; }
    // Get position.
    inline std::size_t pos(void) const noexcept { return (std::size_t)(current - begin); }
    // True if there are no more chars to extract.
    inline bool end(void) const noexcept { return *current == 0; }
    // Jump to the beginning of the stream.
    inline void rewind(void) noexcept { current = begin; }

    virtual ~icsstream(void) {}
};

// Input std::basic_string stream.
class isstream : public imstream
{
public:
    template <typename ...args>
    isstream(const std::basic_string<args...>& str) :
        imstream(str.c_str(), str.length())
    {}
    isstream(isstream&&) = default;
    isstream(const isstream&) = delete;

    isstream& operator=(isstream&&) = default;
    isstream& operator=(const isstream&) = delete;

    virtual ~isstream(void) {}
};

// Output file stream.
class ofstream : internal::fstream_base
{
private:
    const char* filepath;
    unique_file_ptr file;
    std::unique_ptr<char[]> buf;
    char* buf_end;
    char* current;
    std::size_t posn;

public:
    ofstream(const char filepath[], std::size_t bufsize = 256) :
        file(make_unique_file_ptr(filepath, "wb")),
        buf(new char[bufsize]), buf_end(buf.get() + bufsize),
        current(buf.get()), filepath(filepath), posn(0)
    {
        if (bufsize == 0)
            throw std::invalid_argument("Buffer size is 0");
    }
    ofstream(ofstream&&) = default;
    ofstream(const ofstream&) = delete;

    ofstream& operator=(ofstream&&) = default;
    ofstream& operator=(const ofstream&) = delete;

    // Push any buffered chars to target.
    inline void flush(void)
    {
        const std::size_t towrite = (std::size_t)(current - buf.get());
        std::size_t wrote = std::fwrite(buf.get(), sizeof(char), towrite, file.get());
        current = buf.get(); // expected by put()
        if (wrote < towrite || std::fflush(file.get()) == EOF)
            throw std::runtime_error(std::string("Failed to write to ") + filepath);
    }

    // Push a character.
    inline void put(char c) {
        if (current == buf_end) flush();
        *current++ = c; posn++;
    }

    // Get position.
    inline std::size_t pos(void) const noexcept { return posn; }

    // Flush stream and close file.
    // Further usage of the stream is undefined behavior.
    inline void close(void) noexcept
    {
        try { flush(); } catch (...) {}
        file.reset();
    }

    virtual ~ofstream(void) {
        if (file) close();
        // ignore if empty/moved from
    }
};

// Output memory stream.
class omstream
{
protected:
    std::unique_ptr<char[]> buf;
    std::size_t len, cap;

public:
    omstream(std::size_t init_capacity = 4) :
        buf(new char[init_capacity]), cap(init_capacity), len(0)
    {}
    omstream(omstream&&) = default;

    omstream(const omstream& rhs) : 
        buf(new char[rhs.len]), cap(rhs.len), len(rhs.len)
    {
        std::copy(rhs.buf.get(), rhs.buf.get() + rhs.len, buf.get());
    }
    omstream& operator=(const omstream& rhs)
    {
        if (this != &rhs)
        {
            buf.reset(new char[rhs.len]);
            std::copy(rhs.buf.get(), rhs.buf.get() + rhs.len, buf.get());
            len = rhs.len; cap = rhs.len;
        }
        return *this;
    }
    omstream& operator=(omstream&&) = default;
    
    // Push any buffered chars to target.
    inline void flush(void) const noexcept {}

    // Push a character. 
    inline void put(char c) {
        if (len == cap) 
            set_cap(2 * cap + 1);
        buf[len++] = c;
    }

    // Get position.
    inline std::size_t pos(void) const noexcept { return len; }

    // Free unused capacity.
    inline void shrink_to_fit(void) { set_cap(len); }

    // Get length.
    inline std::size_t length(void) const noexcept { return len; }
    // Get capacity.
    inline std::size_t capacity(void) const noexcept { return cap; }
    // Get buffer.
    inline const char* get_buf(void) const noexcept { return buf.get(); }
    // Release (ownership of) buffer.
    inline char* release_buf(void) noexcept { return buf.release(); }

    virtual ~omstream(void) {}

protected:
    inline void set_cap(std::size_t new_cap)
    {
        assert(buf && new_cap >= len);
        if (cap != new_cap)
        {
            char* new_buf = new char[new_cap];
            std::copy(buf.get(), buf.get() + len, new_buf);
            buf.reset(new_buf);
            cap = new_cap;
        }
    }
};

// Fixed size output memory stream.
class omspanstream
{
private:
    const char* begin;
    const char* pend;
    char* current;

public:
    omspanstream(char* src, std::size_t size) :
        begin(src), pend(src + size), current(src)
    {
        if (!src) throw std::invalid_argument("Span is null");
    }
    omspanstream(omspanstream&&) = default;
    omspanstream(const omspanstream&) = delete;

    omspanstream& operator=(omspanstream&&) = default;
    omspanstream& operator=(const omspanstream& rhs) = delete;

    // Push a character. Throws if end().
    inline void put(char c) 
    {
        if (end()) throw std::out_of_range("Span exhausted");
        *current++ = c; 
    }
    // Push any buffered chars to target.
    inline void flush(void) const noexcept {}
    // True if no more chars can be pushed.
    inline bool end(void) const noexcept { return current == pend; }

    virtual ~omspanstream(void) {}
};

// Output C-string stream.
class ocsstream : public omstream
{
public:
    ocsstream(std::size_t init_capacity = 4) :
        omstream(init_capacity)
    {}
    ocsstream(ocsstream&&) = default;
    ocsstream(const ocsstream&) = default;

    ocsstream& operator=(ocsstream&&) = default;
    ocsstream& operator=(const ocsstream& rhs) = default;

    // Convert to C-string and get result.
    inline const char* get_str(void)
    {
        put('\0'); len--;
        return get_buf();
    }
    // Convert to C-string and release ownership.
    inline char* release_str(void)
    {
        set_cap(len + 1); put('\0');
        return release_buf();
    }

    virtual ~ocsstream(void) {}
};

// Output std::string stream.
class osstream
{
private:
    std::string m_str;
public:
    osstream(std::size_t init_capacity = 4)
    { m_str.reserve(init_capacity); }

    osstream(osstream&&) = default;
    osstream(const osstream&) = default;

    osstream& operator=(osstream&&) = default;
    osstream& operator=(const osstream&) = default;

    // Push any buffered chars to target.
    inline void flush(void) const noexcept {};
    // Push a character. 
    inline void put(char c) { m_str.push_back(c); }
    // Get position.
    inline std::size_t pos(void) const noexcept { return m_str.length(); }

    // Get string.
    inline const std::string& get_str(void) const { return m_str; }
    // Release (ownership of) string.
    inline std::string&& release_str(void) { return std::move(m_str); }

    virtual ~osstream(void) {}
};

namespace internal
{
template <typename Iface>
class concat_istream_impl
{
protected:
    std::size_t cur;
    std::vector<Iface> m_streams;

public:
    // Get character. If end(), returns nulchar.
    inline char peek(void) { return current().peek(); }
    // Extract character. If end(), returns nulchar.
    inline char take(void) { return current().take(); }
    // Get position.
    inline std::size_t pos(void) { return current().pos(); }
    // True if there are no more chars to extract.
    inline bool end(void) const noexcept { return cur == m_streams.size(); }

protected:
    inline Iface& current(void)
    {
        for (; cur != m_streams.size(); ++cur)
        {
            auto& is = m_streams[cur];
            if (!is.end()) return is;
        }
        // end of stream placeholder
        return m_streams.back();
    }

    concat_istream_impl(const decltype(m_streams)& streams) :
        cur(0), m_streams(streams)
    {}
    concat_istream_impl(decltype(m_streams)&& streams) :
        cur(0), m_streams(std::move(streams))
    {}
};

using concat_istream_base = concat_istream_impl<interfaces::cistream>;
using rew_concat_istream_base = concat_istream_impl<interfaces::many<interfaces::rewindable, interfaces::cistream>>;
}

// Concatenates a set of input streams.
class concat_istream : public internal::concat_istream_base
{
public:
    template <typename ...istreams>
    concat_istream(istreams&... streams) :
        internal::concat_istream_base({ streams... })
    {}
    concat_istream(concat_istream&&) = default;
    concat_istream(const concat_istream&) = delete;

    concat_istream& operator=(concat_istream&&) = default;
    concat_istream& operator=(const concat_istream&) = delete;

    virtual ~concat_istream(void) {}
};

// Concatenates a set of input streams. Is rewindable.
class rew_concat_istream : public internal::rew_concat_istream_base
{
public:
    template <typename ...istreams>
    rew_concat_istream(istreams&... streams) :
        internal::rew_concat_istream_base({ streams... })
    {}
    rew_concat_istream(rew_concat_istream&&) = default;
    rew_concat_istream(const rew_concat_istream&) = delete;

    rew_concat_istream& operator=(rew_concat_istream&&) = default;
    rew_concat_istream& operator=(const rew_concat_istream&) = delete;

    // Jump to the beginning of the stream.
    inline void rewind(void)
    {
        for (auto& s : m_streams)
            s.rewind();
        cur = 0;
    }

    virtual ~rew_concat_istream(void) {}
};

// Concatenate a set of input streams into a single input stream.
template <typename ...Ts>
inline typename std::conditional<
    internal::conjunction<is_rewindable<Ts>...>::value, 
    rew_concat_istream, concat_istream>::type
concat_istreams(Ts&... streams) { return { streams... }; }


// Adapts a std::basic_istream to this library's istream.
template <typename basic_istream>
class std_istream
{
private:
    basic_istream* stream;
public:
    std_istream(basic_istream& stream) : 
        stream(&stream) 
    {}
    std_istream(std_istream&&) = default;
    std_istream(const std_istream&) = delete;

    std_istream& operator=(std_istream&&) = default;
    std_istream& operator=(const std_istream&) = delete;

    // Get character. If end(), returns nulchar.
    inline char peek(void) { return end() ? '\0' : stream->peek(); }
    // Extract character. If end(), returns nulchar.
    inline char take(void) { return end() ? '\0' : stream->get(); }

    // Get position (only valid if stream was opened in binary mode).
    inline std::size_t pos(void) { return stream->tellg(); }
    // True if there are no more chars to extract.
    inline bool end(void) const { return stream->eof(); }
    // True if last operation on stream failed.
    inline bool fail(void) const { return stream->fail(); }

    // Jump to the beginning of the stream.
    inline void rewind(void) { stream->seekg(0, std::ios_base::beg); }

    virtual ~std_istream(void) {}
};

// Adapts a std::basic_ostream to this library's ostream.
template <typename basic_ostream>
class std_ostream
{
private:
    basic_ostream* stream;
public:
    std_ostream(basic_ostream& stream) :
        stream(&stream) 
    {}
    std_ostream(std_ostream&&) = default;
    std_ostream(const std_ostream&) = delete;

    std_ostream& operator=(std_ostream&&) = default;
    std_ostream& operator=(const std_ostream&) = delete;

    // Push any buffered chars to target.
    inline void flush(void) { stream->flush(); }
    // Push a character.
    inline void put(char c) { stream->put(c); }
    // Get position (only valid if stream was opened in binary mode).
    inline std::size_t pos(void) { return stream->tellp(); }

    // True if last operation on stream failed.
    inline bool fail(void) const { return stream->fail(); }

    virtual ~std_ostream(void) {}
};

using cin_t = std_istream<decltype(std::cin)>;
using cout_t = std_ostream<decltype(std::cout)>;
using cerr_t = std_ostream<decltype(std::cerr)>;
using clog_t = std_ostream<decltype(std::clog)>;

// std::cin adapted to this library's istream concept.
inline cin_t& cin(void) { static cin_t s(std::cin); return s; }
// std::cout adapted to this library's ostream concept.
inline cout_t& cout(void) { static cout_t s(std::cout); return s; }
// std::cerr adapted to this library's ostream concept.
inline cerr_t& cerr(void) { static cerr_t s(std::cerr); return s; }
// std::clog adapted to this library's ostream concept.
inline clog_t& clog(void) { static clog_t s(std::clog); return s; }


enum token_t
{
    TOKEN_eof, // End of stream
    TOKEN_begin_object,
    TOKEN_end_object,
    TOKEN_begin_array,
    TOKEN_end_array,
    TOKEN_key_separator,
    TOKEN_item_separator,
    TOKEN_number,
    TOKEN_string,
    TOKEN_boolean,
    TOKEN_null
};

enum node_t
{
    NODE_array = 1 << 0,
    NODE_object = 1 << 1,
    NODE_key = 1 << 2,
    NODE_value = 1 << 3,
    NODE_document = 1 << 4,
    NODE_NTYPES = 5
};

// Holds a numerical value
// (integral or floating-point).
class number final
{
public:
    enum type_t
    {
        TYPE_float,
        TYPE_double,
        TYPE_int32,
        TYPE_uint32,
        TYPE_int64,
        TYPE_uint64
    };

private:
    type_t m_type;
    union {
        float flt;
        double dbl;
        intl32_t int32;
        uintl32_t uint32;
        intl64_t int64;
        uintl64_t uint64;
    };

    template <type_t type> struct vhelper {};
     
    template <typename least_t>
    static inline void check_least_t_range(least_t value)
    {
        if (value < internal::least_t_exp_min<least_t>::value || 
            value > internal::least_t_exp_max<least_t>::value)
            throw std::invalid_argument("Value out of range.");
    }

public:
    number(intl32_t value) : int32(value), m_type(TYPE_int32) 
    { check_least_t_range(value); }
    number(uintl32_t value) : uint32(value), m_type(TYPE_uint32) 
    { check_least_t_range(value); }
    number(intl64_t value) : int64(value), m_type(TYPE_int64) 
    { check_least_t_range(value); }
    number(uintl64_t value) : uint64(value), m_type(TYPE_uint64) 
    { check_least_t_range(value); }

    number(float value) : flt(value), m_type(TYPE_float) {}
    number(double value) : dbl(value), m_type(TYPE_double) {}

    number() : dbl(0), m_type(TYPE_double) {} // largest range

    // Get type.
    inline type_t type(void) const noexcept { return m_type; }

    // Get value.
    template <type_t type>
    typename vhelper<type>::value_type get(void) const noexcept;

    // Get value, cast to a given type.
    template <type_t type>
    typename vhelper<type>::value_type get_as(void) const noexcept;

    // True if type matches number's type.
    template <type_t type>
    inline bool is(void) const noexcept { return m_type == type; }

    // Get number, cast to a given type.
    template <type_t target_type>
    inline number as(void) const noexcept
    {
        return is<target_type>() ? *this : number(get_as<target_type>());
    }
};

template <> struct number::vhelper<number::TYPE_float> {
    using value_type = float;
    static inline value_type get(const number& n) noexcept { return n.flt; }
};
template <> struct number::vhelper<number::TYPE_double> {
    using value_type = double;
    static inline value_type get(const number& n) noexcept { return n.dbl; }
};
template <> struct number::vhelper<number::TYPE_int32> {
    using value_type = intl32_t;
    static inline value_type get(const number& n) noexcept { return n.int32; }
};
template <> struct number::vhelper<number::TYPE_uint32> {
    using value_type = uintl32_t;
    static inline value_type get(const number& n) noexcept { return n.uint32; }
};
template <> struct number::vhelper<number::TYPE_int64> {
    using value_type = intl64_t;
    static inline value_type get(const number& n) noexcept { return n.int64; }
};
template <> struct number::vhelper<number::TYPE_uint64> {
    using value_type = uintl64_t;
    static inline value_type get(const number& n) noexcept { return n.uint64; }
};

// Get value.
template <number::type_t type>
inline typename number::vhelper<type>::value_type number::get(void) const noexcept
{
    assert(m_type == type);
    return vhelper<type>::get(*this);
}

// Get value, cast to a given type.
template <number::type_t type>
inline typename number::vhelper<type>::value_type number::get_as(void) const noexcept
{
    using value_type = typename vhelper<type>::value_type;
    switch (m_type)
    {
        case TYPE_float: return (value_type)flt;
        case TYPE_double: return (value_type)dbl;
        case TYPE_int32: return (value_type)int32;
        case TYPE_uint32: return (value_type)uint32;
        case TYPE_int64: return (value_type)int64;
        case TYPE_uint64: return (value_type)uint64;
        default: assert(false); return 0;
    }
}

namespace internal
{
// Absolute value of a signed integer.
// Converts to unsigned.
template <typename T, 
    typename uT = typename std::make_unsigned<T>::type>
inline constexpr uT absu(T value) noexcept
{
    // should be true on sane archs :)
    static_assert(
        std::numeric_limits<T>::max() + std::numeric_limits<T>::min() == 0 ||
        std::numeric_limits<uT>::digits > std::numeric_limits<T>::digits, 
        "absU() may fail for values close to signed min()");

    return value < 0 ? -static_cast<uT>(value) : value; 
}

// Negate an unsigned value. Converts to signed.
template <typename T,
    T lbound, T ubound,
    typename uT = typename std::make_unsigned<T>::type>
inline 
#ifdef NDEBUG
constexpr
#endif
T uneg(uT uvalue) noexcept
{
    static_assert(lbound < 0 && ubound > 0 &&
        absu(lbound) >= ubound, "Invalid lower or upper bound");

    assert(uvalue <= absu(lbound));
    return uvalue <= ubound ? -static_cast<T>(uvalue) : 
        -ubound - static_cast<T>(uvalue - ubound);
}

// Negate an unsigned least_t type. Converts to signed.
template <typename T, 
    typename uT = typename std::make_unsigned<T>::type>
inline
#ifdef NDEBUG
constexpr
#endif
T uleast_t_neg(uT value) noexcept
{ return uneg<T, least_t_exp_min<T>::value, least_t_exp_max<T>::value>(value); }

inline bool is_any_of(const char* chars, char val) noexcept
{
    for (char c = *chars++; c != '\0'; c = *chars++) {
        if (c == val) return true;
    }
    return false;
}

inline bool is_digit(char c) noexcept { return c >= '0' && c <= '9'; }

inline char to_lower(char c) noexcept { return c >= 'A' && c <= 'Z' ? c - ('A' - 'a') : c; }

inline void trim_front(std::string& str, const char* needle)
{
    auto off = str.find(needle);
    if (off != std::string::npos)
        str.erase(off, std::strlen(needle));
}

inline void trim_back(std::string& str, const char* needle)
{
    auto off = str.rfind(needle);
    if (off != std::string::npos)
        str.erase(off, std::strlen(needle));
}

template <typename pred>
inline bool starts_with(std::string::iterator str_begin, 
    std::string::iterator str_end, const std::string& needle, pred is_equal)
{
    assert(str_end > str_begin);
    auto str = str_begin;
    for (std::size_t i = 0; i < needle.length(); ++i)
    {
        if (str == str_end) return false;
        if (!is_equal(*str, needle[i])) return false;
        str++;
    }
    return true;
}

inline std::runtime_error parse_error(std::size_t pos, const std::string& message)
{
    return std::runtime_error("Parse error at offset " + std::to_string(pos) + ": " + message);
}
inline std::runtime_error parse_error_exp(std::size_t pos, const std::string& expected)
{
    return parse_error(pos, "expected " + expected);
}

inline bool is_ws(char c) noexcept
{
    switch (c)
    {
        case 0x20: // space
        case 0x09: // horizontal tab
        case 0x0a: // line feed
        case 0x0d: // carriage return
            return true;
        default: return false;
    }
}

// Returns true if stream has more characters.
template <typename ascii_istream>
inline bool skip_ws(ascii_istream& stream)
{
    while (!stream.end() && is_ws(stream.peek())) {
        stream.take();
    }
    return !stream.end();
}

// Returns true if stream has more characters.
template <typename ascii_istream>
inline bool skip_ws(ascii_istream& stream, std::size_t& out_finalpos)
{
    bool rval = skip_ws(stream);
    out_finalpos = stream.pos();
    return rval;
}

template <typename ascii_ostream>
inline void put(ascii_ostream& stream, const char* str) {
    for (char c = *str++; c != '\0'; c = *str++) stream.put(c);
}
template <typename ascii_ostream>
inline void put_repeat(ascii_ostream& stream, char c, std::size_t num_repetitions) {
    for (std::size_t i = 0; i < num_repetitions; ++i) stream.put(c);
}
template <typename ascii_ostream>
inline void put_reverse(ascii_ostream& stream, const std::string& str) 
{
    for (auto i = str.length() - 1; i > 0; --i)
        stream.put(str[i]);
    stream.put(str[0]);
}

template <typename ascii_istream, typename uint_type>
inline bool read_uint(ascii_istream& stream, uint_type& out_value)
{
    if (stream.end() || !is_digit(stream.peek()) 
        || stream.peek() == '-')
        return false;

    out_value = 0;
    while (!stream.end() && is_digit(stream.peek())) {
        uint_type old = out_value;
        out_value = 10 * out_value + (stream.take() - '0');
        if (out_value < old) return false; // overflow
    }
    return true;
}

template <typename int_type, int_type lbound, int_type ubound, typename ascii_istream>
inline bool read_int(ascii_istream& stream, int_type& out_value)
{
    bool neg = stream.peek() == '-';
    if (neg) stream.take();

    typename std::make_unsigned<int_type>::type uvalue;
    if (!read_uint(stream, uvalue)) return false;
    if (neg) {
        if (uvalue > absu(lbound)) return false;
    } else if (uvalue > absu(ubound)) return false;

    out_value = neg ? uneg<int_type, lbound, ubound>(uvalue) : (int_type)uvalue;
    return true;
}

template <typename ascii_istream, typename int_type>
inline bool read_int(ascii_istream& stream, int_type& out_value)
{
    return read_int<int_type, 
        std::numeric_limits<int_type>::min(), 
        std::numeric_limits<int_type>::max()>(stream, out_value);
}

template <typename ascii_ostream, typename uint_type>
inline void write_uint(ascii_ostream& stream, uint_type value)
{
    std::string svalue;
    do {
        // units first
        svalue += '0' + (char)(value % 10);
        value /= 10;
    } while (value != 0);

    put_reverse(stream, svalue);
}

template <typename ascii_ostream, typename int_type>
inline void write_int(ascii_ostream& stream, int_type value)
{
    std::string svalue;
    if (value < 0) stream.put('-');
    do {
        // units first
        auto res = std::div(value, (int_type)10);
        svalue += '0' + (char)std::abs(res.rem);
        value = res.quot;
    } while (value != 0);

    put_reverse(stream, svalue);
}

template <typename ascii_istream>
inline void take_numstr(ascii_istream& stream, std::string& numstr)
{
    while (!stream.end() && !is_ws(stream.peek())
        && !is_any_of(",]}", stream.peek())) {
        numstr += stream.take();
    }
}

template <typename value_type>
inline bool read_floating(std::string&& numstr, value_type& out_value)
{
    if (numstr.length() == 0) return false;

    // skip '-'
    auto numstr_it = numstr.begin();
    if (*numstr_it == '-') {
        numstr_it++;
        if (numstr_it == numstr.end())
            return false;
    }
    auto is_equal_ci = [](char lhs, char rhs) {
        return rhs == to_lower(lhs);
    };
    if (starts_with(numstr_it, numstr.end(), "0x", is_equal_ci) ||
        starts_with(numstr_it, numstr.end(), "nan", is_equal_ci) ||
        starts_with(numstr_it, numstr.end(), "inf", is_equal_ci))
        return false;

    std::istringstream sstream(std::forward<std::string>(numstr));
    sstream.imbue(std::locale::classic()); // make decimal point '.'
    sstream.precision(std::numeric_limits<value_type>::max_digits10);
    sstream >> out_value;

    return !sstream.fail();
}

template <typename ascii_istream, typename value_type>
inline bool read_floating(ascii_istream& stream, value_type& out_value)
{
    std::string numstr; take_numstr(stream, numstr);
    return read_floating(std::move(numstr), out_value);
}

template <typename ascii_ostream, typename value_type>
inline void write_floating(ascii_ostream& stream, value_type value)
{
    if (!std::isfinite(value)) 
        throw std::invalid_argument("Value is NAN or infinity.");

    std::ostringstream sstream;
    sstream.imbue(std::locale::classic()); // make decimal point '.'
    sstream.precision(std::numeric_limits<value_type>::max_digits10);
    sstream << value;

    put(stream, sstream.str().c_str());
}

template <typename ascii_istream>
inline bool read_number(ascii_istream& stream, number& out_value)
{
    auto out_integer = [&](uintl64_t in_value, bool neg) -> bool
    {
        if (neg) {
            if (in_value > absu(int64_min)) return false;
            if (in_value > absu(int32_min))
                out_value = { uleast_t_neg<intl64_t>(in_value) };
            else out_value = { uleast_t_neg<intl32_t>(in_value) };
        }
        else {
            if (in_value > uint64_max) return false;
            if (in_value > int64_max) 
                out_value = { in_value };
            else if (in_value > uint32_max) 
                out_value = { (intl64_t)in_value };
            else if (in_value > int32_max) 
                out_value = { (uintl32_t)in_value };
            else out_value = { (intl32_t)in_value };
        }
        return true;
    };

    auto out_floating = [&](double in_value, bool neg) -> bool
    {
        // todo: with -ffastmath/fp:fast, this may always select float over double
        if (in_value > std::numeric_limits<float>::max() || in_value != (float)in_value)    
            out_value = { in_value * (neg ? -1 : 1) };
        else out_value = { (float)in_value * (neg ? -1.0f : 1.0f) };
        return true;
    };

    std::string numstr;

    bool neg = stream.peek() == '-';
    if (neg) numstr += stream.take();

    if (stream.end() || !is_digit(stream.peek()))
        return false;

    uintl64_t old_int_v, int_v = 0;
    while (!stream.end() && is_digit(stream.peek())) {
        old_int_v = int_v;
        char c = stream.take(); numstr += c;
        int_v = 10 * int_v + (c - '0');
        if (int_v < old_int_v) return false; // overflow
    }
    if (stream.end() || is_ws(stream.peek()))
        return out_integer(int_v, neg);

    switch (stream.peek())
    {
        case ',': case']': case '}':
            return out_integer(int_v, neg);

        case '.': case 'e': case 'E':
        {
            take_numstr(stream, numstr); // remaining
            double float_v;
            bool success = read_floating(std::move(numstr), float_v);
            return success ? out_floating(float_v, neg) : false;
        }

        default: return false;
    }
}

template <typename ascii_ostream>
inline void write_number(ascii_ostream& stream, number value)
{
    switch (value.type())
    {
        case number::TYPE_int32:
            write_int(stream, value.get<number::TYPE_int32>());
            break;
        case number::TYPE_int64:
            write_int(stream, value.get<number::TYPE_int64>());
            break;
        case number::TYPE_uint32:
            write_uint(stream, value.get<number::TYPE_uint32>());
            break;
        case number::TYPE_uint64:
            write_uint(stream, value.get<number::TYPE_uint64>());
            break;
        case number::TYPE_float:
            write_floating(stream, value.get<number::TYPE_float>());
            break;
        case number::TYPE_double:
            write_floating(stream, value.get<number::TYPE_double>());
            break;
        default: assert(false); break;
    }
}

inline const char* try_escape(char c) noexcept
{
    switch (c)
    {
        case '\b': return "\\b";
        case '\f': return "\\f";
        case '\n': return "\\n";
        case '\r': return "\\r";
        case '\t': return "\\t";
        case '"': return "\\\"";
        case '\\': return "\\\\";

        default: return nullptr;
    }
}

bool try_unescape(char c, char& out_c) noexcept
{
    switch (c)
    {
        case 'b': out_c = '\b'; break;
        case 'f': out_c = '\f'; break;
        case 'n': out_c = '\n'; break;
        case 'r': out_c = '\r'; break;
        case 't': out_c = '\t'; break;
        case '"': out_c = '\\'; break;
        case '/': out_c = '/'; break; // MS-only?

        default: return false;
    }
    return true;
};

struct node
{
    static inline std::string desc(unsigned node_types)
    {
        std::string desc;
        for (int i = 0; i < NODE_NTYPES; ++i)
        {
            int type = (node_types & (0x1 << i));
            if (type == 0) continue;

            switch ((node_t)type)
            {
                case NODE_array: desc += ", array"; break;
                case NODE_object: desc += ", object"; break;
                case NODE_key: desc += ", key"; break;
                case NODE_value: desc += ", value"; break;
                case NODE_document: desc += ", document"; break;
                default: assert(false);
            }
        }
        trim_front(desc, ", ");
        return desc;
    }

    bool has_children;
    const node_t type;

    node(node_t type) : 
        type(type), has_children(false)
    {}

    inline bool type_is_any_of(unsigned expected_types) const
    {
        assert(type != 0 && expected_types != 0);
        for (int i = 0; i < NODE_NTYPES; ++i) {
            if (type == (expected_types & (0x1 << i)))
                return true;
        }
        return false;
    }

    inline void assert_type(unsigned allowed_types) const
    {
        if (!type_is_any_of(allowed_types))
            throw std::runtime_error("Expected node: " + node::desc(allowed_types));
    }
};

// True if T = char*, char* const, const char*, or const char* const
template <typename T>
struct is_cv_char_ptr : std::integral_constant<bool,
    std::is_same<typename std::remove_cv<T>::type, char*>::value ||
    std::is_same<typename std::remove_cv<T>::type, const char*>::value>
{};

// True if T is supported string type.
template <typename T>
struct is_string : std::integral_constant<bool,
    is_cv_char_ptr<T>::value || std::is_same<std::string, T>::value>
{};

// forward decl
class template_rw;
}

// Low-level ASCII JSON reader.
template <typename ascii_istream>
class raw_ascii_reader
{
private:
    ascii_istream& stream;
    friend class internal::template_rw;

public:
    raw_ascii_reader(ascii_istream& stream) : stream(stream) {}
    
    // Get current token.
    token_t token(void);

    inline intl32_t read_int32(void) { return read_int<intl32_t>(stream, "int32"); }
    inline intl64_t read_int64(void) { return read_int<intl64_t>(stream, "int64"); }
    inline uintl32_t read_uint32(void) { return read_uint<uintl32_t>(stream, "uint32"); }
    inline uintl64_t read_uint64(void) { return read_uint<uintl64_t>(stream, "uint64"); }
    
    inline float read_float(void) { return read_floating<float>("float"); }
    inline double read_double(void) { return read_floating<double>("double"); }

    number read_number(void);

    bool read_bool(void);
    void read_null(void);

    // Read non-null string into an output stream.
    template <typename ascii_ostream>
    void read_sstream(ascii_ostream& sstream);

    // Read non-null string into an output stream, or return false if value is null.
    // get_sstream_lref = () -> ascii_ostream&.
    template <typename func>
    bool read_sstream_or_null(func get_sstream_lref) 
    { return read_sstream_impl(get_sstream_lref); }

    // Read non-null string.
    inline std::string read_string(void)
    {
        osstream os(4); read_sstream(os);
        return os.release_str();
    }

    // Read null or null-terminated string.
    // String must be delete[]d after usage!
    char* read_cstring_unsafe(std::size_t* out_len = nullptr);

    inline void read_start_object(void) { skip_ws_and_read('{'); } 
    inline void read_end_object(void) { skip_ws_and_read('}'); }
    inline void read_start_array(void) { skip_ws_and_read('['); }
    inline void read_end_array(void) { skip_ws_and_read(']'); }
    inline void read_key_separator(void) { skip_ws_and_read(':'); }
    inline void read_item_separator(void) { skip_ws_and_read(','); }

    // Read arbitrary value.
    template <typename value_type>
    value_type read(void);

    // Skip whitespace and get position of next byte.
    // Returns SIZE_MAX if reached end of stream.
    inline std::size_t pos_after_ws(void)
    { return internal::skip_ws(stream) ? stream.pos() : SIZE_MAX; }

    // Get stream position.
    inline std::size_t pos(void) { return stream.pos(); }

    // True if reached end of stream.
    inline bool end(void) { return stream.end(); }

    virtual ~raw_ascii_reader(void) {}

private:
    void skip_ws_and_read(char expected);

    template <typename int_least_t>
    int_least_t read_int(const char* type_label);
    template <typename uint_least_t>
    uint_least_t read_uint(const char* type_label);
    template <typename float_t>
    float_t read_floating(const char* type_label);

    template <typename func>
    bool read_sstream_impl(func get_sstream_lref, std::size_t* out_startpos = nullptr);
};

// Low-level ASCII JSON writer.
template <typename ascii_ostream>
class raw_ascii_writer
{
private:
    ascii_ostream& stream;
    friend class internal::template_rw;

public:
    raw_ascii_writer(ascii_ostream& stream) : stream(stream) {}

    inline void write_int32(intl32_t value) { internal::write_int(stream, value); }
    inline void write_int64(intl64_t value) { internal::write_int(stream, value); }
    inline void write_uint32(uintl32_t value) { internal::write_uint(stream, value); }
    inline void write_uint64(uintl64_t value) { internal::write_uint(stream, value); }

    inline void write_float(float value) { internal::write_floating(stream, value); }
    inline void write_double(double value) { internal::write_floating(stream, value); }

    inline void write_number(number value) { internal::write_number(stream, value); }

    inline void write_bool(bool value) { internal::put(stream, value ? "true" : "false"); }
    inline void write_null(void) { internal::put(stream, "null"); }

    template <typename ascii_istream>
    void write_sstream(ascii_istream& sstream);

    template <typename func>
    inline void write_sstream_or_null(func get_sstream, bool is_null)
    {
        if (is_null) write_null();
        else {
            auto is = get_sstream();
            write_sstream(is);
        }
    }

    inline void write_string(const char* value) 
    { write_sstream_or_null([&]() -> icsstream { return { value }; }, !value); }

    inline void write_string(const char* value, std::size_t len)
    { write_sstream_or_null([&]() -> imstream { return { value, len }; }, !value); }

    inline void write_string(const std::string& value) 
    {
        imstream is(value.c_str(), value.length());
        write_sstream(is);
    }

    inline void write_start_object(void) { stream.put('{'); }
    inline void write_end_object(void) { stream.put('}'); }
    inline void write_start_array(void) { stream.put('['); }
    inline void write_end_array(void) { stream.put(']'); }
    inline void write_key_separator(void) { stream.put(':'); }
    inline void write_item_separator(void) { stream.put(','); }
    inline void write_newline(void) { stream.put('\n'); }

    inline void write_space(std::size_t num_spaces) 
    { internal::put_repeat(stream, ' ', num_spaces); }

    // Write arbitrary value.
    template <typename value_type> 
    void write(const value_type& value);

    // Flush stream.
    inline void flush(void) { stream.flush(); }

    virtual ~raw_ascii_writer(void) {
        try { flush(); } catch (...) {}
    }
};

namespace internal
{
class rw_base
{
protected:
    std::stack<internal::node> nodes;

    template <unsigned types> void assert_rule(void);

    inline void top_node_add_child(void)
    {
        if (nodes.top().type == NODE_key)
            nodes.pop();
        else nodes.top().has_children = true;
    }

    template <node_t type, typename func>
    inline void start_node(func read_or_write_node)
    {
        assert_rule<type>();
        read_or_write_node();
        nodes.push({ type });
    }

    template <node_t type, typename func>
    inline void end_node(func read_or_write_node)
    {
        nodes.top().assert_type(type);
        nodes.pop();
        read_or_write_node();
        top_node_add_child();
    }
};

template <> inline void rw_base::assert_rule<NODE_array>(void) { nodes.top().assert_type(~NODE_object); }
template <> inline void rw_base::assert_rule<NODE_object>(void) { nodes.top().assert_type(~NODE_object); }
template <> inline void rw_base::assert_rule<NODE_key>(void) { nodes.top().assert_type(NODE_object); }
template <> inline void rw_base::assert_rule<NODE_value>(void) { nodes.top().assert_type(~NODE_object); }
template <> inline void rw_base::assert_rule<NODE_key | NODE_value>(void) { nodes.top().assert_type(NODE_object); }
}

// ASCII JSON reader.
template <typename ascii_istream>
class ascii_reader : public internal::rw_base
{
private:
    ascii_istream& stream;
    raw_ascii_reader<ascii_istream> rrr;

public:
    ascii_reader(ascii_istream& stream) : rrr{ stream }, stream(stream)
    { nodes.push({ NODE_document }); }

    // Get current token.
    inline token_t token(void) { return rrr.token(); }

    // Get the node (e.g. object, array, etc.) that will parent new (key-)values.
    inline node_t parent_node(void) const { return nodes.top().type; }

    inline void start_object(void) { start_node<NODE_object>([&] { read_separator(); rrr.read_start_object(); }); }
    inline void start_array(void) { start_node<NODE_array>([&] { read_separator(); rrr.read_start_array(); }); }
    inline void end_object(void) { end_node<NODE_object>([&] { rrr.read_end_object(); }); }
    inline void end_array(void) { end_node<NODE_array>([&] { rrr.read_end_array(); }); }

    // Read object key.
    inline std::string read_key(void) { return read_key_impl(); }

    // Read object key as null-terminated string.
    // String must be delete[]d after usage!
    char* read_key_unsafe(std::size_t* out_len = nullptr);

    // Read object key.
    // Throws if key does not match expected.
    void read_key(const std::string& expected);

    // Read object key.
    // Throws if key does not match expected.
    void read_key(const char* expected);

    // Read object key.
    // Throws if key does not match expected.
    void read_key(const char* expected, std::size_t exp_len);

    // Read value.
    template <typename value_type>
    value_type read_value(void);

    // Read value.
    template <typename value_type>
    inline void read_value(value_type& out_value)
    { out_value = read_value<value_type>(); }

    // Read string value as null-terminated string.
    // String must be delete[]d after usage!
    inline char* read_strvalue_unsafe(std::size_t* out_len = nullptr);

    // Read object key-value pair.
    template <typename value_type>
    inline std::pair<std::string, value_type> read_key_value(void);

    // Read object key-value pair.
    // Throws if key does not match expected.
    template <typename value_type>
    inline value_type read_key_get_value(const std::string& expected)
    {
        read_key(expected);
        return read_value<value_type>();
    }
    // Read object key-value pair.
    // Throws if key does not match expected.
    template <typename value_type>
    inline value_type read_key_get_value(const char* expected)
    {
        read_key(expected);
        return read_value<value_type>();
    }
    // Read object key-value pair.
    // Throws if key does not match expected.
    template <typename value_type>
    inline value_type read_key_get_value(const char* expected, std::size_t exp_len)
    {
        read_key(expected, exp_len);
        return read_value<value_type>();
    }

    // Read object key-value pair.
    // Throws if key does not match expected.
    template <typename value_type>
    inline void read_key_value(const std::string& expected, value_type& out_value) 
    {
        out_value = read_key_get_value<value_type>(expected);
    }
    // Read object key-value pair.
    // Throws if key does not match expected.
    template <typename value_type>
    inline void read_key_value(const char* expected, value_type& out_value)
    {
        out_value = read_key_get_value<value_type>(expected);
    }
    // Read object key-value pair.
    // Throws if key does not match expected.
    template <typename value_type>
    inline void read_key_value(const char* expected, std::size_t exp_len, value_type& out_value)
    {
        out_value = read_key_get_value<value_type>(expected, exp_len);
    }

    // Get stream position.
    inline std::size_t pos(void) { return stream.pos(); }

    // True if reached end of stream.
    inline bool end(void) { return stream.end(); }

    virtual ~ascii_reader(void) {}

private:
    void read_separator(void);
    std::string read_key_impl(std::size_t* out_pos = nullptr);
};

// ASCII JSON writer.
template <typename ascii_ostream>
class ascii_writer : public internal::rw_base
{
private:
    ascii_ostream& stream;
    raw_ascii_writer<ascii_ostream> rwr;

public:
    ascii_writer(ascii_ostream& stream) : rwr{ stream }, stream(stream)
    { nodes.push({ NODE_document }); }

    // Get the node (e.g. object, array, etc.) that will parent new (key-)values.
    inline node_t parent_node(void) const { return nodes.top().type; }

    void start_object(void) { start_node<NODE_object>([&] { write_separator(); rwr.write_start_object(); }); }
    void start_array(void) { start_node<NODE_array>([&] { write_separator(); rwr.write_start_array(); }); }
    void end_object(void) { end_node<NODE_object>([&] { rwr.write_end_object(); }); }
    void end_array(void) { end_node<NODE_array>([&] { rwr.write_end_array(); }); }

    // Write object key.
    inline void write_key(const char* key) 
    { write_key_impl([&] { rwr.write_string(key); }, !key); }

    // Write object key.
    inline void write_key(const char* key, std::size_t len)
    { write_key_impl([&] { rwr.write_string(key, len); }, !key); }

    // Write object key.
    inline void write_key(const std::string& key) { write_key(key.c_str(), key.length()); }

    // Write value.
    template <typename value_type>
    inline void write_value(const value_type& value)
        // Arrays/function pointers decay to const T*, all other Ts stay the same.
        // See https://stackoverflow.com/questions/12374746/.
    { write_value_impl<typename std::decay<const value_type&>::type>(value); }

    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const char* key, const value_type& value) 
    {
        write_key_value_impl<typename std::decay<const value_type&>::type>(
            [&] { rwr.write_string(key); }, !key, value);
    }
    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const char* key, std::size_t keylen, const value_type& value)
    {
        write_key_value_impl<typename std::decay<const value_type&>::type>(
            [&] { rwr.write_string(key, keylen); }, !key, value);
    }
    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const std::string& key, const value_type& value) 
    { 
        write_key_value(key.c_str(), key.length(), value);
    }
    
    // Write a new line.
    inline void write_newline(void) { stream.put('\n'); }
    // Write indentation.
    inline void write_space(std::size_t num_spaces) { rwr.write_space(num_spaces); }

    // Flush stream.
    inline void flush(void) { stream.flush(); }

    virtual ~ascii_writer(void) {
        try { flush(); } catch (...) {}
    }

private:
    void write_separator(void);
    template <typename func>
    void write_key_impl(func do_write, bool is_null);
    template <typename value_type>
    void write_value_impl(const value_type& value);
    template <typename value_type, typename func>
    void write_key_value_impl(func do_write_key, bool is_key_null, const value_type& value);
};

template <typename ascii_istream>
inline void raw_ascii_reader<ascii_istream>::skip_ws_and_read(char expected)
{
    if (!internal::skip_ws(stream)) goto fail;
    if (stream.peek() != expected) goto fail;
    stream.take();
    return;
fail:
    throw internal::parse_error_exp(stream.pos(),
        std::string("'") + expected + '\'');
}

// Get type of token to be read.
template <typename ascii_istream>
inline token_t raw_ascii_reader<ascii_istream>::token(void)
{
    if (!internal::skip_ws(stream))
        return TOKEN_eof;

    switch (stream.peek())
    {
        case '{': return TOKEN_begin_object;
        case '}': return TOKEN_end_object;
        case '[': return TOKEN_begin_array;
        case ']': return TOKEN_end_array;
        case ':': return TOKEN_key_separator;
        case ',': return TOKEN_item_separator;
        case '"': return TOKEN_string;
        case 't': case 'f': return TOKEN_boolean;
        case 'n': return TOKEN_null;
        default:
            if (internal::is_digit(stream.peek()))
                return TOKEN_number;
            break;
    }
    throw internal::parse_error_exp(stream.pos(), "token");
}

template <typename ascii_istream>
template <typename int_least_t>
inline int_least_t raw_ascii_reader<ascii_istream>::read_int(const char* type_label)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    int_least_t value;
    if (!internal::read_int<int_least_t,
        internal::least_t_exp_min<int_least_t>::value,
        internal::least_t_exp_max<int_least_t>::value>(stream, value)) goto fail;

    return value;
fail:
    throw internal::parse_error_exp(startpos, type_label);
}

template <typename ascii_istream>
template <typename uint_least_t>
inline uint_least_t raw_ascii_reader<ascii_istream>::read_uint(const char* type_label)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    uint_least_t value;
    if (!internal::read_uint(stream, value)) goto fail;
    if (value > internal::least_t_exp_max<uint_least_t>::value) goto fail;

    return value;
fail:
    throw internal::parse_error_exp(startpos, type_label);
}

template <typename ascii_istream>
template <typename float_t>
inline float_t raw_ascii_reader<ascii_istream>::read_floating(const char* type_label)
{
    float_t value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_error_exp(startpos, type_label);
}

template <typename ascii_istream>
inline number raw_ascii_reader<ascii_istream>::read_number(void)
{
    number value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_number(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_error_exp(startpos, "number");
}

template <typename ascii_istream>
inline bool raw_ascii_reader<ascii_istream>::read_bool(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (stream.peek() == 't')
    {
        stream.take();
        if (stream.take() != 'r') goto fail;
        if (stream.take() != 'u') goto fail;
        if (stream.take() != 'e') goto fail;
        return true;
    }
    else if (stream.peek() == 'f')
    {
        stream.take();
        if (stream.take() != 'a') goto fail;
        if (stream.take() != 'l') goto fail;
        if (stream.take() != 's') goto fail;
        if (stream.take() != 'e') goto fail;
        return false;
    }
fail:
    throw internal::parse_error_exp(startpos, "bool");
}

template <typename ascii_istream>
inline void raw_ascii_reader<ascii_istream>::read_null(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (stream.take() != 'n') goto fail;
    if (stream.take() != 'u') goto fail;
    if (stream.take() != 'l') goto fail;
    if (stream.take() != 'l') goto fail;
    return;
fail:
    throw internal::parse_error_exp(startpos, "null");
}

template <typename ascii_istream>
template <typename func>
inline bool
raw_ascii_reader<ascii_istream>::read_sstream_impl(func get_os_lref, std::size_t* out_startpos)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (out_startpos) *out_startpos = startpos;
    if (stream.peek() == 'n')
    {
        stream.take();
        if (stream.take() != 'u') goto fail;
        if (stream.take() != 'l') goto fail;
        if (stream.take() != 'l') goto fail;
        return false;
    }
    else if (stream.peek() == '"')
    {
        auto& os = get_os_lref();

        stream.take(); // open quotes
        while (!stream.end() && stream.peek() != '"')
        {
            switch (stream.peek())
            {
                case '\\':
                {
                    stream.take(); // '\\'
                    char unesc;
                    if (!internal::try_unescape(stream.take(), unesc))
                        throw internal::parse_error(stream.pos() - 1,
                            "Invalid escape char");
                    os.put(unesc);
                }
                break;

                default: os.put(stream.take());
                    break;
            }
        }
        if (stream.end()) goto fail;
        if (stream.take() != '"') goto fail; // close quotes

        return true;
    }
fail:
    throw internal::parse_error_exp(startpos, "string");
}

template <typename ascii_istream>
template <typename ascii_ostream>
inline void raw_ascii_reader<ascii_istream>::read_sstream(ascii_ostream& os)
{
    std::size_t startpos;
    bool is_null = !read_sstream_impl([&]() -> ascii_ostream& { return os; }, &startpos);
    if (is_null)
        throw internal::parse_error_exp(startpos, "non-null string");
}

template <typename ascii_istream>
inline char* raw_ascii_reader<ascii_istream>::read_cstring_unsafe(std::size_t* out_len)
{
    ocsstream os(4);
    bool is_null = !read_sstream_impl([&]() -> ocsstream& { return os; });
    if (is_null) return nullptr;
    if (out_len) *out_len = os.length();
    return os.release_str();
}

template <typename ascii_ostream>
template <typename ascii_istream>
inline void raw_ascii_writer<ascii_ostream>::write_sstream(ascii_istream& is)
{
    stream.put('"');
    while (!is.end())
    {
        char c = is.take();
        const char* esc = internal::try_escape(c);
        if (esc) internal::put(stream, esc);
        else stream.put(c);
    }
    stream.put('"');
}

namespace internal
{
class template_rw
{
public:
    template <typename T, typename ascii_istream, require_t<is_nb_signed_integral<T>::value> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) 
    {
        T value; std::size_t startpos;
        if (!internal::skip_ws(r.stream, startpos)) goto fail;
        if (!internal::read_int(r.stream, value)) goto fail;
        return value;
    fail:
        throw internal::parse_error_exp(startpos,
            std::to_string(std::numeric_limits<T>::digits + 1) +
            "-bit signed integer, typeid: " + typeid(T).name());
    }

    template <typename T, typename ascii_istream, require_t<is_nb_unsigned_integral<T>::value> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) 
    {
        T value; std::size_t startpos;
        if (!internal::skip_ws(r.stream, startpos)) goto fail;
        if (!internal::read_uint(r.stream, value)) goto fail;
        return value;
    fail:
        throw internal::parse_error_exp(startpos,
            std::to_string(std::numeric_limits<T>::digits) +
            "-bit unsigned integer, typeid: " + typeid(T).name());
    }

    template <typename T, typename ascii_istream, require_same_t<T, float> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) { return r.read_float(); }

    template <typename T, typename ascii_istream, require_same_t<T, double> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) { return r.read_double(); }

    template <typename T, typename ascii_istream, require_same_t<T, number> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) { return r.read_number(); }

    template <typename T, typename ascii_istream, require_same_t<T, bool> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) { return r.read_bool(); }

    template <typename T, typename ascii_istream, require_same_t<T, std::string> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) { return r.read_string(); }

    template <typename ascii_istream, typename T, require_same_t<T, std::nullptr_t> = 0>
    static inline T read(raw_ascii_reader<ascii_istream>& r) { r.read_null(); return nullptr; }

    template <typename T, typename ascii_ostream, require_t<is_nb_signed_integral<T>::value> = 0>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, T val) { write_int(w.stream, val); }

    template <typename T, typename ascii_ostream, require_t<is_nb_unsigned_integral<T>::value> = 0>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, T val) { write_uint(w.stream, val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, float val) { w.write_float(val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, double val) { w.write_double(val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, number val) { w.write_number(val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, bool val) { w.write_bool(val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, const char* val) { w.write_string(val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, const std::string& val) { w.write_string(val); }

    template <typename ascii_ostream>
    static inline void write(raw_ascii_writer<ascii_ostream>& w, std::nullptr_t) { w.write_null(); }
};
}

template <typename ascii_istream>
template <typename value_type>
inline value_type raw_ascii_reader<ascii_istream>::read(void)
{
    static_assert(!internal::is_cv_char_ptr<value_type>::value,
        "Templated read does not support C-strings. "
        "Use std::string or read_cstring_unsafe() instead.");
    return internal::template_rw::read<value_type>(*this);
}

template <typename ascii_ostream>
template <typename value_type>
inline void raw_ascii_writer<ascii_ostream>::write(const value_type& value)
{
    internal::template_rw::write(*this, value);
}

namespace internal
{
static constexpr char EX_multi_root[] = "Document cannot have more than one root element.";
}

template <typename ascii_ostream>
inline void ascii_writer<ascii_ostream>::write_separator(void)
{
    if (nodes.top().has_children)
    {
        switch (nodes.top().type)
        {
            case NODE_object:
            case NODE_array: rwr.write_item_separator(); break;
            case NODE_key: rwr.write_key_separator(); break;
            case NODE_document: throw std::runtime_error(internal::EX_multi_root);
            default: assert(false);
        }
    }
}
template <typename ascii_istream>
inline void ascii_reader<ascii_istream>::read_separator(void)
{
    if (nodes.top().has_children)
    {
        switch (nodes.top().type)
        {
            case NODE_object:
            case NODE_array: rrr.read_item_separator(); break;
            case NODE_key: rrr.read_key_separator(); break;
            case NODE_document: throw std::runtime_error(internal::EX_multi_root);
            default: assert(false);
        }
    }
}

template <typename ascii_ostream>
template <typename func>
inline void ascii_writer<ascii_ostream>::write_key_impl(func do_write, bool is_null)
{
    if (is_null) throw std::invalid_argument("Key is null.");
    assert_rule<NODE_key>();

    write_separator();
    do_write();

    top_node_add_child();
    nodes.push({ NODE_key });
}

template <typename ascii_istream>
inline std::string ascii_reader<ascii_istream>::read_key_impl(std::size_t* out_pos)
{
    assert_rule<NODE_key>();

    read_separator();
    if (out_pos) internal::skip_ws(stream, *out_pos);
    std::string str = rrr.read_string();

    top_node_add_child();
    nodes.push({ NODE_key });
    return str;
}

// Read object key as null-terminated string.
// String must be delete[]d after usage!
template <typename ascii_istream>
inline char* ascii_reader<ascii_istream>::read_key_unsafe(std::size_t* out_len)
{
    assert_rule<NODE_key>();

    read_separator();
    std::size_t startpos;
    internal::skip_ws(stream, startpos);
    char* str = rrr.read_cstring_unsafe(out_len);
    if (!str)
        throw internal::parse_error_exp(startpos, "non-null string");

    top_node_add_child();
    nodes.push({ NODE_key });
    return str;
}

// Read object key.
// Throws if key does not match expected.
template <typename ascii_istream>
inline void ascii_reader<ascii_istream>::read_key(const std::string& expected)
{
    std::size_t startpos;
    if (read_key_impl(&startpos) != expected)
        throw internal::parse_error_exp(startpos, "string \"" + expected + "\"");
}

// Read object key.
// Throws if key does not match expected.
template <typename ascii_istream>
inline void ascii_reader<ascii_istream>::read_key(const char* expected)
{
    std::size_t startpos;
    std::string key = read_key_impl(&startpos);
    if (key.compare(0, key.length(), expected) != 0)
        throw internal::parse_error_exp(startpos, "string \"" + std::string(expected) + "\"");
}

// Read object key.
// Throws if key does not match expected.
template <typename ascii_istream>
inline void ascii_reader<ascii_istream>::read_key(const char* expected, std::size_t len)
{
    std::size_t startpos;
    std::string key = read_key_impl(&startpos);
    if (key.compare(0, key.length(), expected, len) != 0)
        throw internal::parse_error_exp(startpos, "string \"" + std::string(expected, len) + "\"");
}

template <typename ascii_ostream>
template <typename value_type>
inline void ascii_writer<ascii_ostream>::write_value_impl(const value_type& value)
{
    assert_rule<NODE_value>();

    write_separator();
    rwr.write(value);

    top_node_add_child();
}

// Read value.
template <typename ascii_istream>
template <typename value_type>
inline value_type ascii_reader<ascii_istream>::read_value(void)
{
    assert_rule<NODE_value>();

    read_separator();
    value_type value = rrr.template read<value_type>();

    top_node_add_child();
    return value;
}

// Read value as null-terminated string.
// String must be delete[]d after usage!
template <typename ascii_istream>
inline char* ascii_reader<ascii_istream>::read_strvalue_unsafe(std::size_t* out_len)
{
    assert_rule<NODE_value>();

    read_separator();
    char* value = rrr.read_cstring_unsafe(out_len);

    top_node_add_child();
    return value;
}

template <typename ascii_ostream>
template <typename value_type, typename func>
inline void ascii_writer<ascii_ostream>::write_key_value_impl(
    func do_write_key, bool is_key_null, const value_type& value)
{
    if (is_key_null) throw std::invalid_argument("Key is null.");
    assert_rule<NODE_key | NODE_value>();

    if (nodes.top().has_children) 
        rwr.write_item_separator();
 
    do_write_key();
    rwr.write_key_separator();
    rwr.write(value);

    top_node_add_child();
}

// Read object key-value pair.
template <typename ascii_istream>
template <typename value_type>
inline std::pair<std::string, value_type> ascii_reader<ascii_istream>::read_key_value(void)
{
    assert_rule<NODE_key | NODE_value>();

    if (nodes.top().has_children) 
        rrr.read_item_separator();

    std::pair<std::string, value_type> ret; // NRVO
    ret.first = rrr.read_string();
    rrr.read_key_separator();
    ret.second = rrr.template read<value_type>();

    top_node_add_child();
    return ret;
}

namespace internal
{
// do_read = (raw_ascii_reader<concat_istream>&) -> <any>
template <typename ascii_istream, typename func>
inline auto from_unquoted_string(ascii_istream& sstream, func do_read) 
-> decltype(do_read(std::declval<raw_ascii_reader<concat_istream>&>()))
{
    icsstream front("\""), back("\"");
    concat_istream is(front, sstream, back);
    raw_ascii_reader<decltype(is)> reader(is);
    return do_read(reader);
}

// do_write = (raw_ascii_writer<osstream>&) -> void
template <typename value_type, typename func>
inline std::string to_unquoted_string(func do_write)
{
    osstream os(4);
    raw_ascii_writer<osstream> writer(os);
    do_write(writer);
    os.flush();

    std::string str = os.release_str();
    if (is_string<value_type>::value)
    {
        trim_front(str, "\"");
        trim_back(str, "\"");
    }
    return str;
}

template <typename value_type, typename ascii_istream, 
    require_t<is_string<value_type>::value> = 0>
inline value_type from_sstream_impl(ascii_istream& sstream)
{
    return from_unquoted_string(sstream, [](raw_ascii_reader<concat_istream>& r) {
        return r.read<value_type>();
    });
}
template <typename value_type, typename ascii_istream, 
    require_t<!is_string<value_type>::value> = 0>
inline value_type from_sstream_impl(ascii_istream& sstream)
{
    raw_ascii_reader<imstream> r(sstream);
    return r.read<value_type>();
}
}

// Convert value to (escaped) string.
template <typename value_type>
inline std::string to_string(const value_type& value)
{
    return internal::to_unquoted_string<value_type>(
        [&](raw_ascii_writer<osstream>& w) { w.write(value); });
}

// Convert string stream to value.
template <typename value_type, typename ascii_istream>
inline value_type from_sstream(ascii_istream& sstream)
{
    static_assert(!internal::is_cv_char_ptr<value_type>::value,
        "Use json::unescape_unsafe() instead");

    return internal::from_sstream_impl<value_type>(sstream);
}

// Escape string stream.
template <typename ascii_istream>
inline std::string escape_sstream(ascii_istream& sstream)
{
    return internal::to_unquoted_string<char*>([&](raw_ascii_writer<osstream>& w) {
        w.write_sstream(sstream);
    });
}

// Escape string stream.
// Output string must be delete[]d after usage!
template <typename ascii_istream>
inline char* escape_sstream_unsafe(ascii_istream& sstream, std::size_t* out_len)
{
    omstream os(4);
    raw_ascii_writer<omstream> writer(os);
    writer.write_sstream(sstream);
    os.flush();

    // trim quotes, add null terminator
    char* estr = new char[os.length() - 1];
    std::copy(os.get_buf() + 1, os.get_buf() + os.length() - 1, estr);
    estr[os.length() - 2] = '\0';

    if (out_len) *out_len = os.length() - 2;
    return estr;
}

// Unescape string stream.
// Output string must be delete[]d after usage!
template <typename ascii_istream>
inline char* unescape_sstream_unsafe(ascii_istream& sstream, std::size_t* out_len)
{
    return internal::from_unquoted_string(sstream, [&](raw_ascii_reader<concat_istream>& r) {
        return r.read_cstring_unsafe(out_len);
    });
}

// Convert string to value.
template <typename value_type>
inline value_type from_string(const char* str, std::size_t len) { imstream is(str, len); return from_sstream<value_type>(is); }
// Convert string to value.
template <typename value_type>
inline value_type from_string(const char* str) { icsstream is(str); return from_sstream<value_type>(is); }
// Convert string to value. 
template <typename value_type>
inline value_type from_string(const std::string& str) { return from_string<value_type>(str.c_str(), str.length()); }


// Escape string.
inline std::string escape(const char* str, std::size_t len) { imstream is(str, len); return escape_sstream(is); }
// Escape string.
inline std::string escape(const char* str) { icsstream is(str); return escape_sstream(is); }
// Escape string.
inline std::string escape(const std::string& str) { return escape(str.c_str(), str.length()); }


// Unescape string.
inline std::string unescape(const char* str, std::size_t len) { return from_string<std::string>(str, len); }
// Unescape string.
inline std::string unescape(const char* str) { return from_string<std::string>(str); }
// Unescape string.
inline std::string unescape(const std::string& str) { return unescape(str.c_str(), str.length()); }


// Escape string.
// Output string must be delete[]d after usage!
inline char* escape_unsafe(const char* str, std::size_t in_len, std::size_t* out_len = nullptr)
{
    imstream sstream(str, in_len); 
    return escape_sstream_unsafe(sstream, out_len);
}
// Escape string.
// Output string must be delete[]d after usage!
inline char* escape_unsafe(const char* str, std::size_t* out_len = nullptr)
{
    icsstream sstream(str); 
    return escape_sstream_unsafe(sstream, out_len);
}

// Unescape string.
// Output string must be delete[]d after usage!
inline char* unescape_unsafe(const char* str, std::size_t in_len, std::size_t* out_len = nullptr)
{
    imstream sstream(str, in_len); 
    return unescape_sstream_unsafe(sstream, out_len);
}
// Unescape string.
// Output string must be delete[]d after usage!
inline char* unescape_unsafe(const char* str, std::size_t* out_len = nullptr)
{
    icsstream sstream(str); 
    return unescape_sstream_unsafe(sstream, out_len);
}

namespace internal
{
template <typename ascii_istream, typename ascii_ostream>
class pretty_printer
{
private:
    unsigned tab_size;
    raw_ascii_reader<ascii_istream> r;
    raw_ascii_writer<ascii_ostream> w;

public:
    pretty_printer(ascii_istream& is, ascii_ostream& os, unsigned tab_size) :
        tab_size(tab_size), r{ is }, w{ os }
    {}

    inline void print(std::size_t depth = 0)
    {
        switch (r.token())
        {
            case TOKEN_begin_object:
            {
                w.write_start_object();
                w.write_newline();

                bool item_sep = false;
                r.read_start_object();
                while (r.token() != TOKEN_end_object)
                {
                    if (item_sep)
                    {
                        r.read_item_separator();
                        w.write_item_separator();
                        w.write_newline();
                    }
                    w.write_space(tab_size * (depth + 1));
                    w.write_string(r.read_string());
                    r.read_key_separator();
                    w.write_key_separator();
                    w.write_space(1);

                    print(depth + 1);
                    item_sep = true;
                }

                r.read_end_object();
                w.write_newline();
                w.write_space(tab_size * depth);
                w.write_end_object();
            }
            break;

            case TOKEN_begin_array:
            {
                w.write_start_array();
                w.write_newline();

                bool item_sep = false;
                r.read_start_array();
                while (r.token() != TOKEN_end_array)
                {
                    if (item_sep)
                    {
                        r.read_item_separator();
                        w.write_item_separator();
                        w.write_newline();
                    }
                    w.write_space(tab_size * (depth + 1));

                    print(depth + 1);
                    item_sep = true;
                }

                r.read_end_array();
                w.write_newline();
                w.write_space(tab_size * depth);
                w.write_end_array();
            }
            break;

            case TOKEN_number:
                w.write_number(r.read_number());
                break;

            case TOKEN_string:
                w.write_string(r.read_string());
                break;

            case TOKEN_boolean:
                w.write_bool(r.read_bool());
                break;

            case TOKEN_null:
                r.read_null();
                w.write_null();
                break;

            case TOKEN_eof: break;
            default: assert(false); break;
        }
    }
};
}

template <typename ascii_istream, typename ascii_ostream>
inline void pretty_print(ascii_istream& in, ascii_ostream& out, unsigned tab_size = 2)
{
    internal::pretty_printer<ascii_istream, ascii_ostream> pp(in, out, tab_size);
    pp.print();
}

template <typename ascii_istream>
inline std::string pretty_print(ascii_istream& stream, unsigned tab_size = 2)
{
    osstream os(4);
    pretty_print(stream, os, tab_size);
    return os.release_str();
}

inline std::string pretty_print(const char* json, std::size_t len, unsigned tab_size = 2)
{
    imstream is(json, len);
    return pretty_print(is, tab_size);
}

inline std::string pretty_print(const char* json, unsigned tab_size = 2)
{
    icsstream is(json);
    return pretty_print(is, tab_size);
}

inline std::string pretty_print(const std::string& json, unsigned tab_size = 2)
{
    return pretty_print(json.c_str(), json.length(), tab_size);
}

}
#endif
