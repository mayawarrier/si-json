
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
#include <sstream>
#include <locale>
#include <vector>
#include <array>
#include <stack>
#include <stdexcept>
#include <type_traits>

namespace json {

namespace internal
{
using unique_file_ptr = std::unique_ptr<std::FILE, void(*)(std::FILE*)>;

inline unique_file_ptr make_unique_file_ptr(const char* filepath, const char* mode)
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

inline std::size_t fread(void* buffer, std::size_t bufsize, 
    std::size_t elemsize, std::size_t count, std::FILE* stream)
{
#ifdef _MSC_VER
    return ::fread_s(buffer, bufsize, elemsize, count, stream);
#else
    (void)bufsize;
    assert(buffer && stream && bufsize >= count * elemsize);
    return std::fread(buffer, elemsize, count, stream);
#endif
}
}

// Input file stream.
class ifstream
{
private:
    const char* filepath;
    internal::unique_file_ptr file;
    std::size_t bufsize;
    std::unique_ptr<char[]> buf;
    char* buf_last_read;
    bool buf_eof;
    char* current;
    std::size_t posn;
    
    // Move ahead (may fill buffer).
    inline void advance(void)
    {
        if (current < buf_last_read) {
            // buffer remaining
            current++; posn++;
        }
        else if (!buf_eof)
        {
            // fill buffer 
            std::size_t nread = internal::fread(buf.get(), 
                bufsize, sizeof(char), bufsize, file.get());
            if (std::ferror(file.get()))
                throw std::runtime_error(std::string("Failed to read from ") + filepath);

            buf_last_read = buf.get() + nread - 1;
            current = buf.get(); posn++;

            if (nread < bufsize) {
                // end of file
                buf[nread] = '\0';
                buf_last_read++;
                buf_eof = true;
            }
        }
    }

public:
    ifstream(const char filepath[], std::size_t bufsize) :
        filepath(filepath), bufsize(bufsize),
        file(internal::make_unique_file_ptr(filepath, "rb")),
        buf(new char[bufsize]), 
        buf_last_read(buf.get()), buf_eof(false),
        current(buf.get()), posn(SIZE_MAX)
    {
        if (bufsize == 0)
            throw std::logic_error("Buffer size must be > 0");
        // fill buffer
        advance();
    }
    ifstream(ifstream&&) = default;

    ifstream& operator=(const ifstream&) = delete;
    ifstream& operator=(ifstream&&) = default;  

    // Get current character.
    inline char peek(void) const noexcept { return *current; }

    // Extract a character.
    inline char take(void) {
        char c = *current;
        advance();
        return c;
    }

    // Get position.
    inline std::size_t pos(void) const noexcept { return posn; }

    // True if no more chars can be taken.
    inline bool end(void) const noexcept 
    { return buf_eof && current == buf_last_read; }

    virtual ~ifstream(void) {}
};

// Output file stream.
class ofstream
{
private:
    const char* filepath;
    internal::unique_file_ptr file;
    std::unique_ptr<char[]> buf;
    char* buf_end;
    char* current;
    
public:
    ofstream(const char filepath[], std::size_t bufsize) :
        file(internal::make_unique_file_ptr(filepath, "wb")),
        buf(new char[bufsize]), buf_end(buf.get() + bufsize),
        current(buf.get()), filepath(filepath)
    {
        if (bufsize == 0)
            throw std::logic_error("Buffer size must be > 0");
    }
    ofstream(ofstream&&) = default;

    ofstream& operator=(const ofstream&) = delete;
    ofstream& operator=(ofstream&&) = default;   

    // Push any buffered data to target.
    inline void flush(void)
    {
        const std::size_t towrite = (std::size_t)(current - buf.get());
        std::size_t wrote = std::fwrite(buf.get(), sizeof(char), towrite, file.get());
        current = buf.get(); // expected by put()
        if (wrote < towrite || std::fflush(file.get()) == EOF) 
            throw std::runtime_error(std::string("Failed to write to ") + filepath);
    }

    // Write a character.
    inline void put(char c) {
        if (current == buf_end) flush();
        *current = c; current++;
    }

    virtual ~ofstream(void) {
        if (file) {
            try { flush(); } catch (...) {}
        } // ignore if empty/moved from
    }
};

// Input memory stream.
class imstream
{
private:
    const char* begin;
    const char* pend;
    const char* current;  
public:
    imstream(const char* src, std::size_t size) :
        begin(src), pend(src + size),
        current(src)
    {}
    imstream(const char* src) : 
        imstream(src, std::strlen(src))
    {}
    imstream(imstream&&) = default;

    imstream& operator=(const imstream&) = delete;
    imstream& operator=(imstream&&) = default;

    // Get current character.
    inline char peek(void) const noexcept { return current < pend ? *current : '\0'; }
    // Get current character then advance.
    inline char take(void) noexcept { return current < pend ? *current++ : '\0'; }
    // Get position.
    inline std::size_t pos(void) const noexcept { return (std::size_t)(current - begin); }
    // True if no more chars can be taken.
    inline bool end(void) const noexcept { return current == pend; }

    // Rewind to the beginning.
    inline void rewind(void) noexcept { current = begin; }

    virtual ~imstream(void) {}
};

// Output memory stream.
class omstream
{
protected:
    std::unique_ptr<char[]> buf;
    std::size_t len, cap;

    void set_cap(std::size_t new_cap)
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
public:
    omstream(std::size_t init_capacity = 0) :
        buf(new char[init_capacity]), cap(init_capacity), len(0)
    {}
    omstream(omstream&&) = default;
    
    omstream& operator=(const omstream&) = delete;
    omstream& operator=(omstream&&) = default;
    
    // Push any buffered data to target.
    inline void flush(void) const noexcept {}

    // Write a character.
    inline void put(char c) {
        if (len == cap) 
            set_cap(2 * cap + 1);
        buf[len++] = c;
    }

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
};

// Wraps a std::string as an input stream.
class isstream : public imstream
{
public:
    isstream(const std::string& str) : 
        imstream(str.c_str(), str.length())
    {}
    isstream(isstream&&) = default;

    isstream& operator=(const isstream&) = delete;
    isstream& operator=(isstream&&) = default;

    virtual ~isstream(void) {}
};

// Wraps a std::string as an output stream.
class osstream
{
private:
    std::string str;
public:
    osstream(std::size_t init_capacity = 0)
    { str.reserve(init_capacity); }

    // Push any buffered data to target.
    inline void flush(void) const noexcept {};
    // Write a character.
    inline void put(char c) { str.push_back(c); }

    // Get string.
    inline const std::string& get_str(void) const { return str; }
    // Move string.
    inline std::string&& move_str(void) { return std::move(str); }
};

// Interface to an arbitrary input stream.
class istream_interface
{
private:
    void* stream;
    char(*peek_impl)(void*);
    char(*take_impl)(void*);
    std::size_t(*pos_impl)(void*);
    bool(*end_impl)(void*);

public:
    template <typename T>
    istream_interface(T& istream) :
        stream(&istream),
        peek_impl([](void* is) -> char { return ((T*)is)->peek(); }), 
        take_impl([](void* is) -> char { return ((T*)is)->take(); }),
        pos_impl([](void* is) -> std::size_t { return ((T*)is)->pos(); }),
        end_impl([](void* is) -> bool { return ((T*)is)->end(); })
    {}

    // Get current character.
    inline char peek(void) const { return peek_impl(stream); }
    // Extract a character.
    inline char take(void) const { return take_impl(stream); }
    // Get position.
    inline std::size_t pos(void) const { return pos_impl(stream); }
    // True if no more chars can be taken.
    inline bool end(void) const { return end_impl(stream); }
};

// Interface to an arbitrary output stream.
class ostream_interface
{
private:
    void* stream;
    void(*flush_impl)(void*);
    void(*put_impl)(void*, char);

public:
    template <typename T>
    ostream_interface(T& stream) :
        stream(&stream),
        flush_impl([](void* os) { ((T*)os)->flush(); }),
        put_impl([](void* os, char c) { ((T*)os)->put(c); })
    {}

    // Write a character.
    inline void put(char c) const { put_impl(stream, c); }
    // Push any buffered data to target.
    inline void flush(void) const { flush_impl(stream); }
};

// Wraps a set of input streams as a single input stream.
class concatenated_istream
{
private:
    std::size_t cur;
    std::vector<istream_interface> streams;

    inline istream_interface& current(void) {
        for (; cur != streams.size(); ++cur)
        {
            auto& is = streams[cur];
            if (!is.end()) return is;
        }
        return streams.back(); // placeholder. end of stream
    }
public:
    concatenated_istream(const std::vector<istream_interface>& streams) : 
        streams(streams), cur(0)
    {}
    concatenated_istream(std::vector<istream_interface>&& streams) :
        streams(std::move(streams)), cur(0)
    {}

    // Get current character.
    inline char peek(void) noexcept { return current().peek(); }
    // Extract a character.
    inline char take(void) noexcept { return current().take(); }
    // Get position.
    inline std::size_t pos(void) noexcept { return current().pos(); }
    // True if stream has ended.
    inline bool end(void) const noexcept { return cur == streams.size(); }
};

// Concatenate a set of input streams into a single input stream.
template <typename ...istreams>
inline concatenated_istream concat_istreams(istreams&... stream)
{
    return { { static_cast<istream_interface>(stream)... } };
}

enum class token_t
{
    begin_object,
    end_object,
    begin_array,
    end_array,
    key_separator,
    item_separator,
    number,
    string,
    boolean,
    null
};

namespace internal
{
constexpr auto int32_max = 0x7FFFFFFF;
constexpr auto int32_min = -0x7FFFFFFF - 1;
constexpr auto uint32_max = 0xFFFFFFFF;

constexpr auto int64_max = 0x7FFFFFFFFFFFFFFF;
constexpr auto int64_min = -0x7FFFFFFFFFFFFFFF - 1;
constexpr auto uint64_max = 0xFFFFFFFFFFFFFFFF;

template <typename T> struct least_t_exp_digits {};
template <> struct least_t_exp_digits<std::int_least32_t>  : std::integral_constant<int, 31> {};
template <> struct least_t_exp_digits<std::int_least64_t>  : std::integral_constant<int, 63> {};
template <> struct least_t_exp_digits<std::uint_least32_t> : std::integral_constant<int, 32> {};
template <> struct least_t_exp_digits<std::uint_least64_t> : std::integral_constant<int, 64> {};

template <typename T> struct least_t_exp_min {};
template <> struct least_t_exp_min<std::int_least32_t>  : std::integral_constant<std::int_least32_t, int32_min> {};
template <> struct least_t_exp_min<std::int_least64_t>  : std::integral_constant<std::int_least64_t, int64_min> {};
template <> struct least_t_exp_min<std::uint_least32_t> : std::integral_constant<std::uint_least32_t, 0> {};
template <> struct least_t_exp_min<std::uint_least64_t> : std::integral_constant<std::uint_least64_t, 0> {};

template <typename T> struct least_t_exp_max {};
template <> struct least_t_exp_max<std::int_least32_t>  : std::integral_constant<std::int_least32_t, int32_max> {};
template <> struct least_t_exp_max<std::int_least64_t>  : std::integral_constant<std::int_least64_t, int64_max> {};
template <> struct least_t_exp_max<std::uint_least32_t> : std::integral_constant<std::uint_least32_t, uint32_max> {};
template <> struct least_t_exp_max<std::uint_least64_t> : std::integral_constant<std::uint_least64_t, uint64_max> {};
}

// Holds a numerical value
// (integral or floating-point).
class number
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
        std::int_least32_t int32;
        std::uint_least32_t uint32;
        std::int_least64_t int64;
        std::uint_least64_t uint64;
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
    number(std::int_least32_t value) : int32(value), m_type(TYPE_int32) 
    { check_least_t_range(value); }
    number(std::uint_least32_t value) : uint32(value), m_type(TYPE_uint32) 
    { check_least_t_range(value); }
    number(std::int_least64_t value) : int64(value), m_type(TYPE_int64) 
    { check_least_t_range(value); }
    number(std::uint_least64_t value) : uint64(value), m_type(TYPE_uint64) 
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
        return is<target_type>() ? *this : (number) { get_as<target_type>() };
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
    using value_type = std::int_least32_t;
    static inline value_type get(const number& n) noexcept { return n.int32; }
};
template <> struct number::vhelper<number::TYPE_uint32> {
    using value_type = std::uint_least32_t;
    static inline value_type get(const number& n) noexcept { return n.uint32; }
};
template <> struct number::vhelper<number::TYPE_int64> {
    using value_type = std::int_least64_t;
    static inline value_type get(const number& n) noexcept { return n.int64; }
};
template <> struct number::vhelper<number::TYPE_uint64> {
    using value_type = std::uint_least64_t;
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
class cstrbuilder : public omstream
{
public:
    cstrbuilder(std::size_t init_capacity = 0) :
        omstream(init_capacity)
    {}
    cstrbuilder(cstrbuilder&&) = default;
    
    cstrbuilder& operator=(const cstrbuilder&) = delete;
    cstrbuilder& operator=(cstrbuilder&&) = default;

    // Get null-terminated string.
    inline const char* get(void) {
        put('\0'); len--;
        return get_buf();
    }
    // Release (ownership of) string.
    inline char* release(void) {
        set_cap(len + 1); put('\0');
        return release_buf();
    }

    // string-like interface
    inline void push_back(char c) { put(c); }
    inline void reserve(std::size_t new_cap) {
        if (new_cap > cap)
            set_cap(new_cap);
    }
};

template <typename str_type>
class string_or_null
{
private:
    bool has_string;
    str_type string;
public:
    string_or_null(std::nullptr_t) :
        has_string(false)
    {}
    string_or_null(str_type&& string) :
        string(std::move(string)), has_string(true)
    {}

    inline bool is_string(void) const noexcept { return has_string; }
    inline bool is_null(void) const noexcept { return !has_string; }

    inline str_type& get_string(void) { assert(has_string); return string; }
    inline str_type&& move_string(void) { assert(has_string); return std::move(string); }
};

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

template <bool test>
using require_t = typename std::enable_if<test, int>::type;

template <typename A, typename B>
using require_same_t = require_t<std::is_same<A, B>::value>;


// Absolute value of a signed integer.
// Converts to unsigned.
// Asserts safety with signed min().
template <typename T, 
    typename uT = typename std::make_unsigned<T>::type, 
    typename = require_t<is_nb_signed_integral<T>::value>>
inline constexpr uT absu(T value) 
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
    typename uT = typename std::make_unsigned<T>::type,
    typename = require_t<is_nb_signed_integral<T>::value>>
inline 
#ifdef NDEBUG
constexpr
#endif
T uneg(uT uvalue)
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
T uleast_t_neg(uT value) 
{ return uneg<T, least_t_exp_min<T>::value, least_t_exp_max<T>::value>(value); }

inline bool is_any_of(const char* chars, char val)
{
    for (char c = *chars++; c != '\0'; c = *chars++) {
        if (c == val) return true;
    }
    return false;
}

inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

inline char to_lower(char c) { return c >= 'A' && c <= 'Z' ? c - ('A' - 'a') : c; }

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
    return str_end > str_begin && needle.length() > 0 && 
        std::search(str_begin, str_end, 
            needle.begin(), needle.end(), is_equal) == str_begin;
}

inline std::runtime_error parse_err(std::size_t position, const char* expected)
{
    return std::runtime_error("Parse error at offset " +
        std::to_string(position) + " - expected " + expected);
}
inline std::runtime_error parse_err(std::size_t position, const std::string& expected)
{
    return parse_err(position, expected.c_str());
}

// Returns true if stream has more characters.
template <typename istream>
inline bool skip_ws(istream& stream)
{
    while (!stream.end() && std::isspace(stream.peek())) {
        stream.take();
    }
    return !stream.end();
}

// Returns true if stream has more characters.
template <typename istream>
inline bool skip_ws(istream& stream, std::size_t& out_finalpos)
{
    bool rval = skip_ws(stream);
    out_finalpos = stream.pos();
    return rval;
}

template <typename ostream>
inline void put(ostream& stream, const char* str) {
    for (char c = *str++; c != '\0'; c = *str++) stream.put(c);
}
template <typename ostream>
inline void put_reverse(ostream& stream, const std::string& str) 
{
    for (auto i = str.length() - 1; i > 0; --i)
        stream.put(str[i]);
    stream.put(str[0]);
}

template <typename istream, typename uint_type>
inline bool read_uint(istream& stream, uint_type& out_value)
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

template <typename int_type, int_type lbound, int_type ubound, typename istream>
inline bool read_int(istream& stream, int_type& out_value)
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

template <typename istream, typename int_type>
inline bool read_int(istream& stream, int_type& out_value)
{
    return read_int<int_type, 
        std::numeric_limits<int_type>::min(), 
        std::numeric_limits<int_type>::max()>(stream, out_value);
}

template <typename uint_type, typename istream>
inline bool read_uint_least_t(istream& stream, uint_type& out_value)
{
    return std::numeric_limits<uint_type>::digits < least_t_exp_digits<uint_type>::value ?
        internal::read_uint(stream, out_value) : // has padding
        internal::read_uint(stream, out_value) && 
            out_value <= least_t_exp_max<uint_type>::value;
}

template <typename int_type, typename istream>
inline bool read_int_least_t(istream& stream, int_type& out_value)
{
    return std::numeric_limits<int_type>::digits < least_t_exp_digits<int_type>::value ?
        internal::read_int(stream, out_value) : // has padding
        internal::read_int<int_type, least_t_exp_min<int_type>::value, 
            least_t_exp_max<int_type>::value>(stream, out_value);
}

template <typename ostream, typename uint_type>
inline void write_uint(ostream& stream, uint_type value)
{
    std::string svalue;
    do {
        // units first
        svalue += '0' + (char)(value % 10);
        value /= 10;
    } while (value != 0);

    put_reverse(stream, svalue);
}

template <typename ostream, typename int_type>
inline void write_int(ostream& stream, int_type value)
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

template <typename istream>
inline void take_numstr(istream& stream, std::string& numstr)
{
    while (!stream.end() && !std::isspace(stream.peek())
        && !is_any_of(",]}", stream.peek())) {
        numstr += stream.take();
    }
}

inline char decimal_point(const std::locale& loc) {
    return std::use_facet<std::numpunct<char>>(loc).decimal_point();
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
    auto case_insensitive_pred = [](char lhs, char rhs) {
        return rhs == to_lower(lhs);
    };
    if (starts_with(numstr_it, numstr.end(), "0x", case_insensitive_pred) ||
        starts_with(numstr_it, numstr.end(), "nan", case_insensitive_pred) ||
        starts_with(numstr_it, numstr.end(), "inf", case_insensitive_pred))
        return false;

    std::istringstream sstream(std::forward<std::string>(numstr));
    assert(decimal_point(std::locale::classic()) == '.');
    sstream.imbue(std::locale::classic()); // make decimal point '.'
    sstream.precision(std::numeric_limits<value_type>::max_digits10);
    sstream >> out_value;

    return !sstream.fail();
}

template <typename istream, typename value_type>
inline bool read_floating(istream& stream, value_type& out_value)
{
    std::string numstr; take_numstr(stream, numstr);
    return read_floating(std::move(numstr), out_value);
}

template <typename ostream, typename value_type>
inline void write_floating(ostream& stream, value_type value)
{
    if (!std::isfinite(value)) 
        throw std::invalid_argument("Value is NAN or infinity.");

    std::ostringstream sstream;
    assert(decimal_point(std::locale::classic()) == '.');
    sstream.imbue(std::locale::classic()); // make decimal point '.'
    sstream.precision(std::numeric_limits<value_type>::max_digits10);
    sstream << value;

    put(stream, sstream.str().c_str());
}

template <typename istream>
inline bool read_number(istream& stream, json::number& out_value)
{
    auto out_integer = [&](std::uint_least64_t in_value, bool neg) -> bool
    {
        if (neg) {
            if (in_value > absu(int64_min)) return false;
            if (in_value > absu(int32_min))
                out_value = { uleast_t_neg<std::int_least64_t>(in_value) };
            else out_value = { uleast_t_neg<std::int_least32_t>(in_value) };
        }
        else {
            if (in_value > uint64_max) return false;
            if (in_value > int64_max) 
                out_value = { in_value };
            else if (in_value > uint32_max) 
                out_value = { (std::int_least64_t)in_value };
            else if (in_value > int32_max) 
                out_value = { (std::uint_least32_t)in_value };
            else out_value = { (std::int_least32_t)in_value };
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

    std::uint_least64_t old_int_v, int_v = 0;
    while (!stream.end() && is_digit(stream.peek())) {
        old_int_v = int_v;
        char c = stream.take(); numstr += c;
        int_v = 10 * int_v + (c - '0');
        if (int_v < old_int_v) return false; // overflow
    }
    if (stream.end()) 
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

template <typename ostream>
inline void write_number(ostream& stream, json::number value)
{
    switch (value.type())
    {
        case json::number::TYPE_int32:
            write_int(stream, value.get<json::number::TYPE_int32>());
            break;
        case json::number::TYPE_int64:
            write_int(stream, value.get<json::number::TYPE_int64>());
            break;
        case json::number::TYPE_uint32:
            write_uint(stream, value.get<json::number::TYPE_uint32>());
            break;
        case json::number::TYPE_uint64:
            write_uint(stream, value.get<json::number::TYPE_uint64>());
            break;
        case json::number::TYPE_float:
            write_floating(stream, value.get<json::number::TYPE_float>());
            break;
        case json::number::TYPE_double:
            write_floating(stream, value.get<json::number::TYPE_double>());
            break;
        default: assert(false); break;
    }
}

template <typename istream>
inline char take_unescape(istream& stream)
{
    assert(stream.peek() == '\\');
    stream.take();

    char value = stream.take();
    switch (value)
    {
        case 'b': return '\b';
        case 'f': return '\f';
        case 'n': return '\n';
        case 'r': return '\r';
        case 't': return '\t';
        case '"': case '\\':
        case '/': // MS-only?
            return value;

        default: throw parse_err(stream.pos() - 1,
            "one of b, f, n, r, t, \", \\, or /");
    }
}

inline const char* try_escape(char c)
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

template <typename istream>
inline void skip_ws_and_read(istream& stream, char expected)
{
    if (!skip_ws(stream)) goto fail;
    if (stream.peek() != expected) goto fail;
    stream.take();
    return;
fail:
    throw parse_err(stream.pos(), 
        std::string("'") + expected + '\'');
}

enum node_type
{
    NODE_array = 1 << 0,
    NODE_object = 1 << 1,
    NODE_key = 1 << 2,
    NODE_value = 1 << 3,
    NODE_document = 1 << 4,
    NODE_NTYPES = 5
};

class node
{
    static inline std::string desc(unsigned node_types)
    {
        std::string desc;
        for (auto i = 0; i < NODE_NTYPES; ++i)
        {
            int type = (node_types & (0x1 << i));
            if (type == 0) continue;

            switch ((node_type)type)
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

private:
    bool m_has_children;
public:
    const node_type type;

    node(node_type type) : 
        type(type), m_has_children(false)
    {}

    inline bool type_is_any_of(unsigned expected_types) const
    {
        assert(type != 0 && expected_types != 0);
        for (auto i = 0; i < NODE_NTYPES; ++i) {
            if (type == (expected_types & (0x1 << i)))
                return true;
        }
        return false;
    }

    inline void assert_type(unsigned allowed_types)
    {
        if (!type_is_any_of(allowed_types))
            throw std::logic_error("Expected node: " + node::desc(allowed_types));
    }

    template <unsigned types> void assert_rule(void);

    inline bool has_children(void) const noexcept { return m_has_children; }

    template <unsigned types> 
    static inline void add_child(std::stack<node>& nodes)
    { nodes.top().m_has_children = true; }
};

template <> inline void node::assert_rule<NODE_array>(void) { assert_type(~NODE_object); }
template <> inline void node::assert_rule<NODE_object>(void) { assert_type(~NODE_object); }
template <> inline void node::assert_rule<NODE_key>(void) { assert_type(NODE_object); }
template <> inline void node::assert_rule<NODE_value>(void) { assert_type(~NODE_object); }
template <> inline void node::assert_rule<NODE_key | NODE_value>(void) { assert_type(NODE_object); }

template <> void node::add_child<NODE_value>(std::stack<node>& nodes)
{
    if (nodes.top().type == internal::NODE_key)
        nodes.pop();
    else nodes.top().m_has_children = true;
}

// T(&)[] -> T*, T[] -> T*
template <typename T>
struct decay_array
{
    using rref_t = typename std::remove_reference<T>::type;
    using type = typename std::conditional<
        std::is_array<rref_t>::value,
        typename std::remove_extent<rref_t>::type*, T
    >::type;
};
template <typename T>
using decay_array_t = typename decay_array<T>::type;

// True if T = char*, char* const, const char*, or const char* const
template <typename T>
struct is_cv_char_ptr : std::integral_constant<bool,
    std::is_same<typename std::remove_cv<T>::type, char*>::value ||
    std::is_same<typename std::remove_cv<T>::type, const char*>::value>
{};

// True if T is supported (maybe cv-qualified) string type.
template <typename T>
struct is_cv_string : std::integral_constant<bool,
    is_cv_char_ptr<T>::value || 
    std::is_same<std::string, typename std::remove_cv<T>::type>::value>
{};

// forward decl
class rw;
}

// Low-level ASCII JSON reader.
template <typename istream>
class raw_reader
{
private:
    istream& stream;
    friend class internal::rw;

    // str_type must be movable and implement reserve() and push_back().
    template <typename str_type>
    internal::string_or_null<str_type>
        read_string_variant(std::size_t* out_startpos = nullptr);

public:
    raw_reader(istream& stream) : stream(stream) {}
    virtual ~raw_reader(void) {}

    // Get next token type.
    token_t token(void);

    std::int_least32_t read_int32(void);
    std::int_least64_t read_int64(void);
    std::uint_least32_t read_uint32(void);
    std::uint_least64_t read_uint64(void);
    
    float read_float(void);
    double read_double(void);

    json::number read_number(void);

    bool read_bool(void);
    void read_null(void);

    // Read non-null string.
    std::string read_string(void);

    // Read null or null-terminated string.
    // String must be delete[]d after usage!
    char* read_cstring_unsafe(std::size_t* out_len = nullptr);

    inline void read_start_object(void) { internal::skip_ws_and_read(stream, '{'); } 
    inline void read_end_object(void) { internal::skip_ws_and_read(stream, '}'); }
    inline void read_start_array(void) { internal::skip_ws_and_read(stream, '['); }
    inline void read_end_array(void) { internal::skip_ws_and_read(stream, ']'); }
    inline void read_key_separator(void) { internal::skip_ws_and_read(stream, ':'); }
    inline void read_item_separator(void) { internal::skip_ws_and_read(stream, ','); }

    // Read arbitrary value.
    template <typename value_type>
    value_type read(void);

    // Get stream position.
    inline std::size_t pos(void) { return stream.pos(); }

    // Get position of first char that is not whitespace.
    // Returns SIZE_MAX if reached end of stream.
    inline std::size_t skip_ws_pos(void)
    { return internal::skip_ws(stream) ? stream.pos() : SIZE_MAX; }

    // True if reached end of stream.
    inline bool end(void) { return stream.end(); }
};

// Low-level ASCII JSON writer.
template <typename ostream>
class raw_writer
{
private:
    ostream& stream;
    friend class internal::rw;
public:
    raw_writer(ostream& stream) : stream(stream) {}

    inline void write_int32(std::int_least32_t value) { internal::write_int(stream, value); }
    inline void write_int64(std::int_least64_t value) { internal::write_int(stream, value); }
    inline void write_uint32(std::uint_least32_t value) { internal::write_uint(stream, value); }
    inline void write_uint64(std::uint_least64_t value) { internal::write_uint(stream, value); }

    inline void write_float(float value) { internal::write_floating(stream, value); }
    inline void write_double(double value) { internal::write_floating(stream, value); }

    inline void write_number(json::number value) { internal::write_number(stream, value); }

    inline void write_bool(bool value) { internal::put(stream, value ? "true" : "false"); }
    inline void write_null(void) { internal::put(stream, "null"); }

    // is_end = (std::size_t index) -> bool,
    // returns true when past end of string.
    template <typename func>
    void write_string(const char* value, func is_end);

    inline void write_string(const char* value)
    { write_string(value, [&](std::size_t i) -> bool { return value[i] == '\0'; }); }

    inline void write_string(const char* value, std::size_t len)
    { write_string(value, [&](std::size_t i) -> bool { return i == len; }); }

    inline void write_string(const std::string& value) 
    { write_string(value.c_str(), value.length()); }

    inline void write_start_object(void) { stream.put('{'); }
    inline void write_end_object(void) { stream.put('}'); }
    inline void write_start_array(void) { stream.put('['); }
    inline void write_end_array(void) { stream.put(']'); }
    inline void write_key_separator(void) { stream.put(':'); }
    inline void write_item_separator(void) { stream.put(','); }

    // Write arbitrary value.
    template <typename value_type> 
    void write(const value_type& value);

    // Flush stream.
    inline void flush(void) { stream.flush(); }

    virtual ~raw_writer(void) {
        try { flush(); } catch (...) {}
    }
};

// ASCII JSON reader.
template <typename istream>
class reader
{
private:
    istream& stream;
    raw_reader<istream> rrr;
    std::stack<internal::node> nodes;

    void maybe_read_separator(void);
    std::string read_key_impl(std::size_t* out_pos = nullptr);

public:
    reader(istream& stream) : rrr{ stream }, stream(stream)
    { nodes.push({ internal::NODE_document }); }

    void start_object(void);
    void start_array(void);
    void end_object(void);
    void end_array(void);

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

    virtual ~reader(void) {}
};

// ASCII JSON writer.
template <typename ostream>
class writer
{
private:
    ostream& stream;
    raw_writer<ostream> rwr;
    std::stack<internal::node> nodes;

    void maybe_write_separator(void);
    template <typename func>
    void write_key_impl(const char* key, func is_end);
    template <typename value_type>
    void write_value_impl(const value_type& value);
    template <typename value_type, typename func>
    void write_key_value_impl(const char* key, func is_end, const value_type& value);

public:
    writer(ostream& stream) : rwr{ stream }, stream(stream)
    { nodes.push({ internal::NODE_document }); }

    void start_object(void);
    void start_array(void);
    void end_object(void);
    void end_array(void);

    // Write object key.
    inline void write_key(const char* key) 
    { write_key_impl(key, [&](std::size_t i) -> bool { return key[i] == '\0'; }); }

    // Write object key.
    inline void write_key(const char* key, std::size_t len)
    { write_key_impl(key, [&](std::size_t i) -> bool { return i == len; }); }

    // Write object key.
    inline void write_key(const std::string& key) { write_key(key.c_str(), key.length()); }

    // Write value.
    template <typename value_type>
    inline void write_value(const value_type& value)
    { write_value_impl((internal::decay_array_t<const value_type&>)value); }

    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const char* key, const value_type& value) 
    { 
        write_key_value_impl(key, 
            [&](std::size_t i) -> bool { return key[i] == '\0'; },
            (internal::decay_array_t<const value_type&>)value);
    }
    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const char* key, std::size_t keylen, const value_type& value)
    {
        write_key_value_impl(key,
            [&](std::size_t i) -> bool { return i == keylen; },
            (internal::decay_array_t<const value_type&>)value);
    }
    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const std::string& key, const value_type& value) 
    { 
        write_key_value_impl(key.c_str(),
            [&](std::size_t i) -> bool { return i == key.length(); },
            (internal::decay_array_t<const value_type&>)value);
    }

    // Flush stream.
    inline void flush(void) { stream.flush(); }

    virtual ~writer(void) {
        assert(nodes.size() == 1 && nodes.top().type == 
            internal::NODE_document && "Incomplete JSON.");
        try { flush(); } catch (...) {}
    }
};

namespace internal
{
// do_read = (raw_reader<concatenated_istream>&) -> <any>
template <typename func>
inline auto from_unquoted_string(const char* str, std::size_t len, func do_read) 
-> decltype(do_read(std::declval<raw_reader<concatenated_istream>&>()))
{
    imstream front("\""), middle(str, len), back("\"");
    auto is = concat_istreams(front, middle, back);
    raw_reader<decltype(is)> reader(is);
    return do_read(reader);
}

// do_write = (raw_writer<osstream>&) -> void
template <typename value_type, typename func>
inline std::string to_unquoted_string(func do_write)
{
    osstream os(4);
    raw_writer<osstream> writer(os);
    do_write(writer);
    os.flush();

    std::string str = os.move_str();
    if (is_cv_string<value_type>::value)
    {
        trim_front(str, "\"");
        trim_back(str, "\"");
    }
    return str;
}

template <typename value_type, require_t<is_cv_string<value_type>::value> = 0>
inline value_type from_string_impl(const char* str, std::size_t len)
{
    return from_unquoted_string(str, len,
        [](raw_reader<concatenated_istream>& r) { return r.read<value_type>(); });
}

template <typename value_type, require_t<!is_cv_string<value_type>::value> = 0>
inline value_type from_string_impl(const char* str, std::size_t len)
{
    imstream is(str, len);
    raw_reader<imstream> reader(is);
    return reader.read<value_type>();
}
}

// Convert value to (escaped) string.
template <typename value_type>
inline std::string to_string(const value_type& value)
{
    return internal::to_unquoted_string<value_type>(
        [&](raw_writer<osstream>& w) { w.write(value); });
}

// Convert string to value.
template <typename value_type>
inline value_type from_string(const char* str, std::size_t len)
{ 
    static_assert(!internal::is_cv_char_ptr<value_type>::value,
        "Use json::unescape_unsafe() instead");

    return internal::from_string_impl<value_type>(str, len);
}

// Convert string to value. 
template <typename value_type>
inline value_type from_string(const std::string& str)
{ 
    return from_string<value_type>(str.c_str(), str.length()); 
}
// Convert string to value.
template <typename value_type>
inline void from_string(const std::string& str, value_type& out_value)
{ 
    out_value = from_string<value_type>(str); 
}
// Convert string to value.
template <typename value_type>
inline void from_string(const char* str, std::size_t len, value_type& out_value)
{ 
    out_value = from_string<value_type>(str, len); 
}
// Convert string to value.
template <typename value_type>
inline value_type from_string(const char* str)
{ 
    return from_string<value_type>(str, std::strlen(str)); 
}
template <typename value_type>
inline void from_string(const char* str, value_type& out_value)
{ 
    out_value = from_string<value_type>(str); 
}

// Escape string (by JSON escape rules).
inline std::string escape(const char* str, std::size_t len)
{
    return internal::to_unquoted_string<char*>(
        [&](raw_writer<osstream>& w) { w.write_string(str, len); });
}
// Escape string (by JSON escape rules).
inline std::string escape(const char* str)
{
    return escape(str, std::strlen(str));
}
// Escape string (by JSON escape rules).
inline std::string escape(const std::string& str) 
{
    return escape(str.c_str(), str.length());
}
// Unescape string (by JSON unescape rules).
inline std::string unescape(const char* str, std::size_t len)
{
    return from_string<std::string>(str, len);
}
// Unescape string (by JSON unescape rules).
inline std::string unescape(const char* str)
{
    return unescape(str, std::strlen(str));
}
// Unescape string (by JSON unescape rules).
inline std::string unescape(const std::string& str)
{
    return unescape(str.c_str(), str.length());
}

// Escape string (by JSON escape rules).
// Output string must be delete[]d after usage!
inline char* escape_unsafe(const char* str, std::size_t in_len, std::size_t* out_len = nullptr)
{
    omstream os(4);
    raw_writer<omstream> writer(os);
    writer.write_string(str, in_len);
    os.flush();

    os.shrink_to_fit();
    char* estr = os.release_buf();
    // overwrite quotes, null-terminate
    estr[os.length() - 1] = '\0';
    std::memmove(estr, estr + 1, os.length() - 1);

    if (out_len) *out_len = os.length() - 2;
    return estr;
}

// Escape string (by JSON escape rules).
// Output string must be delete[]d after usage!
inline char* escape_unsafe(const char* str, std::size_t* out_len = nullptr)
{
    return escape_unsafe(str, std::strlen(str), out_len);
}

// Unescape string (by JSON unescape rules).
// Output string must be delete[]d after usage!
inline char* unescape_unsafe(const char* str, std::size_t in_len, std::size_t* out_len = nullptr)
{
    return internal::from_unquoted_string(str, in_len,
        [&](raw_reader<concatenated_istream>& r) { return r.read_cstring_unsafe(out_len); });
}

// Unescape string (by JSON unescape rules).
// Output string must be delete[]d after usage!
inline char* unescape_unsafe(const char* str, std::size_t* out_len = nullptr)
{
    return unescape_unsafe(str, std::strlen(str), out_len);
}

template <typename ostream>
inline void writer<ostream>::maybe_write_separator(void)
{
    if (nodes.top().has_children())
    {
        switch (nodes.top().type)
        {
            case internal::NODE_document: stream.put(' '); break; // RFC 7159
            case internal::NODE_object:
            case internal::NODE_array: rwr.write_item_separator(); break;
            case internal::NODE_key: rwr.write_key_separator(); break;
            default: assert(false);
        }
    }
}
template <typename istream>
inline void reader<istream>::maybe_read_separator(void)
{
    if (nodes.top().has_children())
    {
        switch (nodes.top().type)
        {
            case internal::NODE_document: /* ws always skipped */ break; // RFC 7159
            case internal::NODE_object:
            case internal::NODE_array: rrr.read_item_separator(); break;
            case internal::NODE_key: rrr.read_key_separator(); break;
            default: assert(false);
        }
    }
}

template <typename ostream>
inline void writer<ostream>::start_object(void)
{
    nodes.top().assert_rule<internal::NODE_object>();
    maybe_write_separator();
    rwr.write_start_object();
    nodes.push({ internal::NODE_object });
}
template <typename istream>
inline void reader<istream>::start_object(void)
{
    nodes.top().assert_rule<internal::NODE_object>();
    maybe_read_separator();
    rrr.read_start_object();
    nodes.push({ internal::NODE_object });
}
template <typename ostream>
inline void writer<ostream>::start_array(void)
{
    nodes.top().assert_rule<internal::NODE_array>();
    maybe_write_separator();
    rwr.write_start_array();
    nodes.push({ internal::NODE_array });
}
template <typename istream>
inline void reader<istream>::start_array(void)
{
    nodes.top().assert_rule<internal::NODE_array>();
    maybe_read_separator();
    rrr.read_start_array();
    nodes.push({ internal::NODE_array });
}
template <typename ostream>
inline void writer<ostream>::end_object(void)
{
    nodes.top().assert_type(internal::NODE_object); 
    nodes.pop();
    rwr.write_end_object();
    internal::node::add_child<internal::NODE_object>(nodes);
}
template <typename istream>
inline void reader<istream>::end_object(void)
{
    nodes.top().assert_type(internal::NODE_object); 
    nodes.pop();
    rrr.read_end_object();
    internal::node::add_child<internal::NODE_object>(nodes);
}
template <typename ostream>
inline void writer<ostream>::end_array(void)
{
    nodes.top().assert_type(internal::NODE_array); 
    nodes.pop();
    rwr.write_end_array();
    internal::node::add_child<internal::NODE_array>(nodes);
}
template <typename istream>
inline void reader<istream>::end_array(void)
{
    nodes.top().assert_type(internal::NODE_array); 
    nodes.pop();
    rrr.read_end_array();
    internal::node::add_child<internal::NODE_array>(nodes);
}

// Write object key.
template <typename ostream>
template <typename func>
inline void writer<ostream>::write_key_impl(const char* key, func is_end)
{
    assert(key);
    nodes.top().assert_rule<internal::NODE_key>();

    maybe_write_separator();
    rwr.write_string(key, is_end);

    internal::node::add_child<internal::NODE_key>(nodes);
    nodes.push({ internal::NODE_key });
}

template <typename istream>
inline std::string reader<istream>::read_key_impl(std::size_t* out_pos)
{
    nodes.top().assert_rule<internal::NODE_key>();

    maybe_read_separator();
    if (out_pos) internal::skip_ws(stream, *out_pos);
    std::string str = rrr.read_string();

    internal::node::add_child<internal::NODE_key>(nodes);
    nodes.push({ internal::NODE_key });
    return str;
}

// Read object key as null-terminated string.
// String must be delete[]d after usage!
template <typename istream>
inline char* reader<istream>::read_key_unsafe(std::size_t* out_len)
{
    nodes.top().assert_rule<internal::NODE_key>();

    maybe_read_separator();
    std::size_t startpos;
    internal::skip_ws(stream, startpos);
    char* str = rrr.read_cstring_unsafe(out_len);
    if (!str)
        throw internal::parse_err(startpos, "non-null string");

    internal::node::add_child<internal::NODE_key>(nodes);
    nodes.push({ internal::NODE_key });
    return str;
}

// Read object key.
// Throws if key does not match expected.
template <typename istream>
inline void reader<istream>::read_key(const std::string& expected)
{
    std::size_t startpos;
    if (read_key_impl(&startpos) != expected)
        throw internal::parse_err(startpos, "string \"" + expected + "\"");
}

// Read object key.
// Throws if key does not match expected.
template <typename istream>
inline void reader<istream>::read_key(const char* expected)
{
    std::size_t startpos;
    std::string key = read_key_impl(&startpos);
    if (key.compare(0, key.length(), expected) != 0)
        throw internal::parse_err(startpos, "string \"" + std::string(expected) + "\"");
}

// Read object key.
// Throws if key does not match expected.
template <typename istream>
inline void reader<istream>::read_key(const char* expected, std::size_t len)
{
    std::size_t startpos;
    std::string key = read_key_impl(&startpos);
    if (key.compare(0, key.length(), expected, len) != 0)
        throw internal::parse_err(startpos, "string \"" + std::string(expected, len) + "\"");
}

template <typename ostream>
template <typename value_type>
inline void writer<ostream>::write_value_impl(const value_type& value)
{
    nodes.top().assert_rule<internal::NODE_value>();

    maybe_write_separator();
    rwr.write(value);

    internal::node::add_child<internal::NODE_value>(nodes);
}

// Read value.
template <typename istream>
template <typename value_type>
inline value_type reader<istream>::read_value(void)
{
    nodes.top().assert_rule<internal::NODE_value>();

    maybe_read_separator();
    value_type value = rrr.template read<value_type>();

    internal::node::add_child<internal::NODE_value>(nodes);
    return value;
}

// Read value as null-terminated string.
// String must be delete[]d after usage!
template <typename istream>
inline char* reader<istream>::read_strvalue_unsafe(std::size_t* out_len)
{
    nodes.top().assert_rule<internal::NODE_value>();

    maybe_read_separator();
    char* value = rrr.read_cstring_unsafe(out_len);

    internal::node::add_child<internal::NODE_value>(nodes);
    return value;
}

template <typename ostream>
template <typename value_type, typename func>
inline void writer<ostream>::write_key_value_impl(const char* key, func is_end, const value_type& value)
{
    assert(key);
    nodes.top().assert_rule<internal::NODE_key | internal::NODE_value>();

    if (nodes.top().has_children()) 
        rwr.write_item_separator();
 
    rwr.write_string(key, is_end);
    rwr.write_key_separator();
    rwr.write(value);

    internal::node::add_child<internal::NODE_key | internal::NODE_value>(nodes);
}

// Read object key-value pair.
template <typename istream>
template <typename value_type>
inline std::pair<std::string, value_type> reader<istream>::read_key_value(void)
{
    nodes.top().assert_rule<internal::NODE_key | internal::NODE_value>();

    if (nodes.top().has_children()) 
        rrr.read_item_separator();

    std::pair<std::string, value_type> ret; // NRVO
    ret.first = rrr.read_string();
    rrr.read_key_separator();
    ret.second = rrr.template read<value_type>();

    internal::node::add_child<internal::NODE_key | internal::NODE_value>(nodes);
    return ret;
}

template <typename istream>
inline std::int_least32_t raw_reader<istream>::read_int32(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::int_least32_t value;
    if (!internal::read_int_least_t(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "int32");
}

template <typename istream>
inline std::int_least64_t raw_reader<istream>::read_int64(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::int_least64_t value;
    if (!internal::read_int_least_t(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "int64");
}

template <typename istream>
inline std::uint_least32_t raw_reader<istream>::read_uint32(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::uint_least32_t value;
    if (!internal::read_uint_least_t(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "uint32");
}

template <typename istream>
inline std::uint_least64_t raw_reader<istream>::read_uint64(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::uint_least64_t value;
    if (!internal::read_uint_least_t(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "uint64");
}

template <typename istream>
inline float raw_reader<istream>::read_float(void)
{
    float value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "float");
}

template <typename istream>
inline double raw_reader<istream>::read_double(void)
{
    double value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "double");
}

template <typename istream>
inline json::number raw_reader<istream>::read_number(void)
{
    json::number value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_number(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "number");
}

template <typename istream>
inline bool raw_reader<istream>::read_bool(void)
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
    throw internal::parse_err(startpos, "bool");
}

template <typename istream>
inline void raw_reader<istream>::read_null(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (stream.take() != 'n') goto fail;
    if (stream.take() != 'u') goto fail;
    if (stream.take() != 'l') goto fail;
    if (stream.take() != 'l') goto fail;
fail:
    throw internal::parse_err(startpos, "null");
}

template <typename istream>
template <typename str_type>
inline internal::string_or_null<str_type>
raw_reader<istream>::read_string_variant(std::size_t* out_startpos)
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
        return { nullptr };
    }
    else if (stream.peek() == '"')
    {
        stream.take(); // open quotes
        str_type str; str.reserve(4);
        while (!stream.end() && stream.peek() != '"')
        {
            switch (stream.peek())
            {
                case '\\':
                    str.push_back(internal::take_unescape(stream));
                    break;
                default:
                    str.push_back(stream.take());
                    break;
            }
        }
        if (stream.end()) goto fail;
        if (stream.take() != '"') goto fail; // close quotes

        return { std::move(str) };
    }
fail:
    throw internal::parse_err(startpos, "string");
}

template <typename istream>
inline std::string raw_reader<istream>::read_string(void)
{
    std::size_t startpos;
    auto&& maybe_string = read_string_variant<std::string>(&startpos);
    if (maybe_string.is_null())
        throw internal::parse_err(startpos, "non-null string");
    return maybe_string.move_string();
}

template <typename istream>
inline char* raw_reader<istream>::read_cstring_unsafe(std::size_t* out_len)
{
    auto&& maybe_string = read_string_variant<internal::cstrbuilder>();
    if (maybe_string.is_string())
    {
        auto& sb = maybe_string.get_string();
        if (out_len) *out_len = sb.length();
        return sb.release();
    }
    else return nullptr;
}

template <typename ostream>
template <typename func>
inline void raw_writer<ostream>::write_string(const char* value, func is_end)
{
    if (value == nullptr)
        internal::put(stream, "null");
    else
    {
        stream.put('"');
        for (std::size_t i = 0; !is_end(i); ++i)
        {
            char c = value[i];
            const char* esc = internal::try_escape(c);
            if (esc) internal::put(stream, esc);
            else stream.put(c);
        }
        stream.put('"');
    }
}

// Get type of token to be read.
template <typename istream>
inline token_t raw_reader<istream>::token(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    switch (stream.peek())
    {
        case '{': return token_t::begin_object;
        case '}': return token_t::end_object;
        case '[': return token_t::begin_array;
        case ']': return token_t::end_array;
        case ':': return token_t::key_separator;
        case ',': return token_t::item_separator;
        case '"': return token_t::string;
        case 't': case 'f': return token_t::boolean;
        case 'n': return token_t::null;
        default:
            if (internal::is_digit(stream.peek()))
                return token_t::number;
            break;
    }
fail:
    throw internal::parse_err(stream.pos(), "token");
}

namespace internal
{
class rw
{
private:
    template <typename istream, typename T>
    static inline T read_int_impl(istream& stream) 
    { 
        T value; std::size_t startpos;
        if (!internal::skip_ws(stream, startpos)) goto fail;
        if (!internal::read_int(stream, value)) goto fail;
        return value;
    fail:
        throw internal::parse_err(startpos,
            std::to_string(std::numeric_limits<T>::digits + 1) +
            "-bit signed integer, typeid: " + typeid(T).name());
    }

    template <typename istream, typename T>
    static inline T read_uint_impl(istream& stream)
    {
        T value; std::size_t startpos;
        if (!internal::skip_ws(stream, startpos)) goto fail;
        if (!internal::read_uint(stream, value)) goto fail;
        return value;
    fail:
        throw internal::parse_err(startpos,
            std::to_string(std::numeric_limits<T>::digits) +
            "-bit unsigned integer, typeid: " + typeid(T).name());
    }

public:
    template <typename T, typename istream, require_t<is_nb_signed_integral<T>::value> = 0>
    static inline T read(raw_reader<istream>& r) { return read_int_impl<istream, T>(r.stream); }

    template <typename T, typename istream, require_t<is_nb_unsigned_integral<T>::value> = 0>
    static inline T read(raw_reader<istream>& r) { return read_uint_impl<istream, T>(r.stream); }

    template <typename T, typename istream, require_same_t<T, float> = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_float(); }

    template <typename T, typename istream, require_same_t<T, double> = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_double(); }

    template <typename T, typename istream, require_same_t<T, json::number> = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_number(); }

    template <typename T, typename istream, require_same_t<T, bool> = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_bool(); }

    template <typename T, typename istream, require_same_t<typename std::remove_cv<T>::type, std::string> = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_string(); }

    template <typename istream, typename T, require_same_t<T, std::nullptr_t> = 0>
    static inline T read(raw_reader<istream>& r) { r.read_null(); return nullptr; }

    template <typename T, typename ostream, require_t<is_nb_signed_integral<T>::value> = 0>
    static inline void write(raw_writer<ostream>& w, T val) { write_int(w.stream, val); }

    template <typename T, typename ostream, require_t<is_nb_unsigned_integral<T>::value> = 0>
    static inline void write(raw_writer<ostream>& w, T val) { write_uint(w.stream, val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, float val) { w.write_float(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, double val) { w.write_double(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, json::number val) { w.write_number(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, bool val) { w.write_bool(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, const char* val) { w.write_string(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, const std::string& val) { w.write_string(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, std::nullptr_t) { w.write_null(); }
};
}

template <typename istream> 
template <typename value_type>
inline value_type raw_reader<istream>::read(void)
{
    static_assert(!internal::is_cv_char_ptr<value_type>::value,
        "Templated read does not support C-strings. "
        "Use std::string or call read_cstring_unsafe() instead.");
    return internal::rw::read<value_type>(*this);
}

template <typename ostream> 
template <typename value_type> 
inline void raw_writer<ostream>::write(const value_type& value) 
{
    internal::rw::write(*this, 
        (internal::decay_array_t<const value_type&>)value);
}
}

#endif
