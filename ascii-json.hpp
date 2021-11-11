
#ifndef ASCII_JSON_HPP
#define ASCII_JSON_HPP

#include <cstddef>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdint>
#include <cmath>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <utility>
#include <algorithm> // std::copy
#include <memory>
#include <string>
#include <sstream>
#include <vector>
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

    // True if stream has ended.
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

    // Commit data to target.
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
    // True if stream has ended.
    inline bool end(void) const noexcept { return current == pend; }

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
    
    // Commit data to target.
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

// Wraps a std::string as an output stream.
class osstream
{
private:
    std::string str;
public:
    osstream(std::size_t init_capacity = 0)
    { str.reserve(init_capacity); }

    // Commit data to target.
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
    // True if stream has ended.
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
    // Commit data to target.
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

// Absolute value of a signed integer.
// Safe to use with signed min().
template <typename T, 
    typename uT = typename std::make_unsigned<T>::type, 
    typename = typename std::enable_if<is_nb_signed_integral<T>::value>::type>
inline constexpr uT absu(T value) 
{
    // should be true on sane archs :)
    static_assert(static_cast<T>(
        std::numeric_limits<uT>::max() +
        std::numeric_limits<T>::min()) >= 0, 
        "absU() may fail for values close to signed min()");

    return value < 0 ? -((uT)value) : value; 
}

inline bool is_any_of(const char* chars, char val)
{
    for (char c = *chars++; c != '\0'; c = *chars++) {
        if (c == val) return true;
    }
    return false;
}

inline bool is_digit(char c) { return c >= '0' && c <= '9'; }

inline void trim_front(std::string& str, const char* target)
{
    auto off = str.find(target);
    if (off != std::string::npos)
        str.erase(off, std::strlen(target));
}
inline void trim_back(std::string& str, const char* target)
{
    auto off = str.rfind(target);
    if (off != std::string::npos)
        str.erase(off, std::strlen(target));
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

template <typename istream, typename int_type>
inline bool read_int(istream& stream, 
    const int_type lbound, const int_type ubound, int_type& out_value)
{
    using uint_type = typename std::make_unsigned<int_type>::type;

    bool neg = stream.peek() == '-';
    if (neg) stream.take();

    uint_type uvalue;
    if (!read_uint(stream, uvalue)) return false;
    if (neg) {
        if (uvalue > absu(lbound)) return false;
    } else if (uvalue > absu(ubound)) return false;

    out_value = (int_type)uvalue * (neg ? -1 : 1);
    return true;
}

template <typename istream, typename int_type>
inline bool read_int(istream& stream, int_type& out_value)
{
    return read_int(stream, 
        std::numeric_limits<int_type>::min(), 
        std::numeric_limits<int_type>::max(), 
        out_value);
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
        auto res = std::div(value, 10);
        svalue += '0' + (char)std::abs(res.rem);
        value = res.quot;
    } while (value != 0);

    put_reverse(stream, svalue);
}

template <typename T>
using fp_conv_t = T(*)(const char*, char**);

template <typename T> struct fp_converter {};

template <> struct fp_converter<float> : 
    std::integral_constant<fp_conv_t<float>, std::strtof> 
{};
template <> struct fp_converter<double> :
    std::integral_constant<fp_conv_t<double>, std::strtod>
{};
template <> struct fp_converter<long double> :
    std::integral_constant<fp_conv_t<long double>, std::strtold>
{};

template <typename istream, typename value_type>
inline bool read_floating(istream& stream, value_type& out_value)
{
    std::string sval;
    while (!stream.end() && !std::isspace(stream.peek())
        && !is_any_of(",]}", stream.peek())) {
        sval += stream.take();
    }
    if (sval.length() == 0) return false;

    char* v_end;
    const char* v_begin = sval.c_str();
    out_value = fp_converter<value_type>::value(v_begin, &v_end);

    // failed to decode?
    if (v_end != v_begin + sval.length())
        return false;

    // too big?
    if (errno == ERANGE) {
        errno = 0;
        return false;
    }
    return true;
}

template <typename ostream, typename value_type>
inline void write_floating(ostream& stream, value_type value)
{
    std::ostringstream sstream;
    sstream.precision(std::numeric_limits<value_type>::max_digits10);
    sstream << value;
    put(stream, sstream.str().c_str());
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
        (std::string("'") + expected + '\'').c_str());
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

class node_t
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

    node_t(node_type type) : 
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
            throw std::logic_error("Expected node: " + node_t::desc(allowed_types));
    }

    template <unsigned types> inline void assert_rule(void);

    template <> void assert_rule<NODE_array>(void) { assert_type(~NODE_object); }
    template <> void assert_rule<NODE_object>(void) { assert_type(~NODE_object); }
    template <> void assert_rule<NODE_key>(void) { assert_type(NODE_object); }
    template <> void assert_rule<NODE_value>(void) { assert_type(~NODE_object); }
    template <> void assert_rule<NODE_key | NODE_value>(void) { assert_type(NODE_object); }

    inline bool has_children(void) const noexcept { return m_has_children; }

    template <unsigned types> 
    static inline void add_child(std::stack<node_t>& nodes)
    { nodes.top().m_has_children = true; }
};

template <> void node_t::add_child<NODE_value>(std::stack<node_t>& nodes)
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
    std::is_same<std::string, 
        typename std::remove_cv<T>::type
    >::value>
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
    inline internal::string_or_null<str_type>
        read_string_variant(std::size_t* out_startpos = nullptr);

public:
    raw_reader(istream& stream) : stream(stream) {}
    virtual ~raw_reader(void) {}

    inline std::int_least32_t read_int32(void);
    inline std::int_least64_t read_int64(void);
    inline std::uint_least32_t read_uint32(void);
    inline std::uint_least64_t read_uint64(void);

    inline float read_float(void);
    inline double read_double(void);
    inline bool read_bool(void);
    inline void read_null(void);

    // Read non-null string.
    inline std::string read_string(void);

    // Read null or null-terminated string.
    // String must be delete[]d after usage!
    inline char* read_cstring_unsafe(std::size_t* out_len = nullptr);

    inline void read_start_object(void) { internal::skip_ws_and_read(stream, '{'); } 
    inline void read_end_object(void) { internal::skip_ws_and_read(stream, '}'); }
    inline void read_start_array(void) { internal::skip_ws_and_read(stream, '['); }
    inline void read_end_array(void) { internal::skip_ws_and_read(stream, ']'); }
    inline void read_key_separator(void) { internal::skip_ws_and_read(stream, ':'); }
    inline void read_item_separator(void) { internal::skip_ws_and_read(stream, ','); }

    // Read arbitrary value.
    template <typename value_type>
    inline value_type read(void);

    // Skip whitespace. 
    // Called at the beginning of every read.
    inline void skip_ws(void) { internal::skip_ws(stream); }

    // Synonym for stream.peek().
    inline char peek(void) { return stream.peek(); }
    // Synonym for stream.take().
    inline char take(void) { return stream.take(); }  
    // Synonym for stream.pos().
    inline std::size_t pos(void) { return stream.pos(); }
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
    inline void write_bool(bool value) { internal::put(stream, value ? "true" : "false"); }
    inline void write_null(void) { internal::put(stream, "null"); }

    // is_end = bool(*)(std::size_t index);
    // returns true when past end of the string.
    template <typename func>
    inline void write_string(const char* value, func is_end);

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
    inline void write(const value_type& value);

    // Synonym for stream.put().
    inline void put(char c) { stream.put(c); }
    // Synonym for stream.flush().
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
    raw_reader<istream> rrr;
    std::stack<internal::node_t> nodes;

    inline void maybe_read_separator(void);
    inline std::string read_key_impl(std::size_t* out_pos = nullptr);

public:
    reader(istream& stream) : rrr{ stream }
    { nodes.push({ internal::NODE_document }); }

    inline void start_object(void);
    inline void start_array(void);
    inline void end_object(void);
    inline void end_array(void);

    // Read object key.
    inline std::string read_key(void) { return read_key_impl(); }

    // Read object key as null-terminated string.
    // String must be delete[]d after usage!
    inline char* read_key_unsafe(std::size_t* out_len = nullptr);

    // Read object key.
    // Throws if key does not match expected.
    inline void read_key(const std::string& expected);

    // Read object key.
    // Throws if key does not match expected.
    inline void read_key(const char* expected);

    // Read object key.
    // Throws if key does not match expected.
    inline void read_key(const char* expected, std::size_t exp_len);

    // Read value.
    template <typename value_type>
    inline value_type read_value(void);

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

    // Synonym for stream.pos().
    inline std::size_t pos(void) { return rrr.pos(); }

    virtual ~reader(void) {}
};

// ASCII JSON writer.
template <typename ostream>
class writer
{
private:
    raw_writer<ostream> rwr;
    std::stack<internal::node_t> nodes;

    inline void maybe_write_separator(void);
    template <typename func>
    inline void write_key_impl(const char* key, func is_end);
    template <typename value_type>
    inline void write_value_impl(const value_type& value);
    template <typename value_type, typename func>
    inline void write_key_value_impl(const char* key, func is_end, const value_type& value);

public:
    writer(ostream& stream) : rwr{ stream }
    { nodes.push({ internal::NODE_document }); }

    inline void start_object(void);
    inline void start_array(void);
    inline void end_object(void);
    inline void end_array(void);

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
            [&](std::size_t i) -> bool { return i == len; },
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

    // Synonym for stream.flush().
    inline void flush(void) { rwr.flush(); }

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
-> decltype(do_read(std::declval<raw_reader<concatenated_istream>>()))
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
    writer.flush();

    std::string str = os.move_str();
    if (is_cv_string<value_type>::value)
    {
        trim_front(str, "\"");
        trim_back(str, "\"");
    }
    return str;
}

template <typename value_type,
    typename std::enable_if<is_cv_string<value_type>::value, int>::type = 0>
inline value_type from_string_impl(const char* str, std::size_t len)
{
    return from_unquoted_string(str, len,
        [](raw_reader<concatenated_istream>& r) { return r.read<value_type>(); });
}

template <typename value_type,
    typename std::enable_if<!is_cv_string<value_type>::value, int>::type = 0>
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
    writer.flush();

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
void writer<ostream>::maybe_write_separator(void)
{
    if (nodes.top().has_children())
    {
        switch (nodes.top().type)
        {
            case internal::NODE_document: rwr.put(' '); break; // RFC 7159
            case internal::NODE_object:
            case internal::NODE_array: rwr.write_item_separator(); break;
            case internal::NODE_key: rwr.write_key_separator(); break;
            default: assert(false);
        }
    }
}
template <typename istream>
void reader<istream>::maybe_read_separator(void)
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
void writer<ostream>::start_object(void)
{
    nodes.top().assert_rule<internal::NODE_object>();
    maybe_write_separator();
    rwr.write_start_object();
    nodes.push({ internal::NODE_object });
}
template <typename istream>
void reader<istream>::start_object(void)
{
    nodes.top().assert_rule<internal::NODE_object>();
    maybe_read_separator();
    rrr.read_start_object();
    nodes.push({ internal::NODE_object });
}
template <typename ostream>
void writer<ostream>::start_array(void)
{
    nodes.top().assert_rule<internal::NODE_array>();
    maybe_write_separator();
    rwr.write_start_array();
    nodes.push({ internal::NODE_array });
}
template <typename istream>
void reader<istream>::start_array(void)
{
    nodes.top().assert_rule<internal::NODE_array>();
    maybe_read_separator();
    rrr.read_start_array();
    nodes.push({ internal::NODE_array });
}
template <typename ostream>
void writer<ostream>::end_object(void)
{
    nodes.top().assert_type(internal::NODE_object); 
    nodes.pop();
    rwr.write_end_object();
    internal::node_t::add_child<internal::NODE_object>(nodes);
}
template <typename istream>
void reader<istream>::end_object(void)
{
    nodes.top().assert_type(internal::NODE_object); 
    nodes.pop();
    rrr.read_end_object();
    internal::node_t::add_child<internal::NODE_object>(nodes);
}
template <typename ostream>
void writer<ostream>::end_array(void)
{
    nodes.top().assert_type(internal::NODE_array); 
    nodes.pop();
    rwr.write_end_array();
    internal::node_t::add_child<internal::NODE_array>(nodes);
}
template <typename istream>
void reader<istream>::end_array(void)
{
    nodes.top().assert_type(internal::NODE_array); 
    nodes.pop();
    rrr.read_end_array();
    internal::node_t::add_child<internal::NODE_array>(nodes);
}

// Write object key.
template <typename ostream>
template <typename func>
void writer<ostream>::write_key_impl(const char* key, func is_end)
{
    assert(key);
    nodes.top().assert_rule<internal::NODE_key>();

    maybe_write_separator();
    rwr.write_string(key, is_end);

    internal::node_t::add_child<internal::NODE_key>(nodes);
    nodes.push({ internal::NODE_key });
}

template <typename istream>
std::string reader<istream>::read_key_impl(std::size_t* out_pos)
{
    nodes.top().assert_rule<internal::NODE_key>();

    maybe_read_separator();
    if (out_pos) {
        rrr.skip_ws();
        *out_pos = rrr.pos();
    }
    std::string str = rrr.read_string();

    internal::node_t::add_child<internal::NODE_key>(nodes);
    nodes.push({ internal::NODE_key });
    return str;
}

// Read object key as null-terminated string.
// String must be delete[]d after usage!
template <typename istream>
char* reader<istream>::read_key_unsafe(std::size_t* out_len)
{
    nodes.top().assert_rule<internal::NODE_key>();

    maybe_read_separator();
    rrr.skip_ws();
    std::size_t startpos = rrr.pos();
    char* str = rrr.read_cstring_unsafe(out_len);
    if (!str)
        throw internal::parse_err(startpos, "non-null string");

    internal::node_t::add_child<internal::NODE_key>(nodes);
    nodes.push({ internal::NODE_key });
    return str;
}

// Read object key.
// Throws if key does not match expected.
template <typename istream>
void reader<istream>::read_key(const std::string& expected)
{
    std::size_t startpos;
    if (read_key_impl(&startpos) != expected)
        throw internal::parse_err(startpos, "string \"" + expected + "\"");
}

// Read object key.
// Throws if key does not match expected.
template <typename istream>
void reader<istream>::read_key(const char* expected)
{
    std::size_t startpos;
    std::string key = read_key_impl(&startpos);
    if (key.compare(0, key.length(), expected) != 0)
        throw internal::parse_err(startpos, "string \"" + std::string(expected) + "\"");
}

// Read object key.
// Throws if key does not match expected.
template <typename istream>
void reader<istream>::read_key(const char* expected, std::size_t len)
{
    std::size_t startpos;
    std::string key = read_key_impl(&startpos);
    if (key.compare(0, key.length(), expected, len) != 0)
        throw internal::parse_err(startpos, "string \"" + std::string(expected, len) + "\"");
}

template <typename ostream>
template <typename value_type>
void writer<ostream>::write_value_impl(const value_type& value)
{
    nodes.top().assert_rule<internal::NODE_value>();

    maybe_write_separator();
    rwr.write(value);

    internal::node_t::add_child<internal::NODE_value>(nodes);
}

// Read value.
template <typename istream>
template <typename value_type>
value_type reader<istream>::read_value(void)
{
    nodes.top().assert_rule<internal::NODE_value>();

    maybe_read_separator();
    value_type value = rrr.read<value_type>();

    internal::node_t::add_child<internal::NODE_value>(nodes);
    return value;
}

// Read value as null-terminated string.
// String must be delete[]d after usage!
template <typename istream>
char* reader<istream>::read_strvalue_unsafe(std::size_t* out_len)
{
    nodes.top().assert_rule<internal::NODE_value>();

    maybe_read_separator();
    char* value = rrr.read_cstring_unsafe(out_len);

    internal::node_t::add_child<internal::NODE_value>(nodes);
    return value;
}

template <typename ostream>
template <typename value_type, typename func>
void writer<ostream>::write_key_value_impl(const char* key, func is_end, const value_type& value)
{
    assert(key);
    nodes.top().assert_rule<internal::NODE_key | internal::NODE_value>();

    if (nodes.top().has_children()) 
        rwr.write_item_separator();
 
    rwr.write_string(key, is_end);
    rwr.write_key_separator();
    rwr.write(value);

    internal::node_t::add_child<internal::NODE_key | internal::NODE_value>(nodes);
}

// Read object key-value pair.
template <typename istream>
template <typename value_type>
std::pair<std::string, value_type> reader<istream>::read_key_value(void)
{
    nodes.top().assert_rule<internal::NODE_key | internal::NODE_value>();

    if (nodes.top().has_children()) 
        rrr.read_item_separator();

    std::pair<std::string, value_type> ret; // NRVO
    ret.first = rrr.read_string();
    rrr.read_key_separator();
    ret.second = rrr.read<value_type>();

    internal::node_t::add_child<internal::NODE_key | internal::NODE_value>(nodes);
    return ret;
}

template <typename istream>
std::int_least32_t raw_reader<istream>::read_int32(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::int_least32_t value;
    if (std::numeric_limits<std::int_least32_t>::digits < 31) { 
        // theoretically possible due to padding bits 
        if (!internal::read_int(stream, value)) goto fail;
    }
    else if (!internal::read_int(stream,
        -0x80000000, 0x7FFFFFFF, value)) goto fail;

    return value;
fail:
    throw internal::parse_err(startpos, "int32");
}

template <typename istream>
std::int_least64_t raw_reader<istream>::read_int64(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::int_least64_t value;
    if (std::numeric_limits<std::int_least64_t>::digits < 63) {
        // theoretically possible due to padding bits 
        if (!internal::read_int(stream, value)) goto fail;
    }
    else if (!internal::read_int(stream, 
        -0x8000000000000000, 0x7FFFFFFFFFFFFFFF, value)) goto fail;

    return value;
fail:
    throw internal::parse_err(startpos, "int64");
}

template <typename istream>
std::uint_least32_t raw_reader<istream>::read_uint32(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    std::uint_least32_t value;
    if (std::numeric_limits<std::uint_least32_t>::digits < 32) {
        // theoretically possible due to padding bits 
        if (!internal::read_uint(stream, value)) goto fail;
    }
    else if (!internal::read_uint(stream, value) 
        || value > 0xFFFFFFFF) goto fail;

    return value;
fail:
    throw internal::parse_err(startpos, "uint32");
}

template <typename istream>
std::uint_least64_t raw_reader<istream>::read_uint64(void)
{
    std::uint_least64_t value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;

    if (std::numeric_limits<uint_least64_t>::digits < 64) {
        // theoretically possible due to padding bits 
        if (!internal::read_uint(stream, value)) goto fail;
    }
    else if (!internal::read_uint(stream, value) 
        || value > 0xFFFFFFFFFFFFFFFF) goto fail;

    return value;
fail:
    throw internal::parse_err(startpos, "uint64");
}

template <typename istream>
float raw_reader<istream>::read_float(void)
{
    float value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "float");
}

template <typename istream>
double raw_reader<istream>::read_double(void)
{
    double value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "double");
}

template <typename istream>
bool raw_reader<istream>::read_bool(void)
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
void raw_reader<istream>::read_null(void)
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
raw_reader<istream>::read_string_variant(std::size_t* out_startpos = nullptr)
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
std::string raw_reader<istream>::read_string(void)
{
    std::size_t startpos;
    auto&& maybe_string = read_string_variant<std::string>(&startpos);
    if (maybe_string.is_null())
        throw internal::parse_err(startpos, "non-null string");
    return maybe_string.move_string();
}

template <typename istream>
char* raw_reader<istream>::read_cstring_unsafe(std::size_t* out_len)
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
void raw_writer<ostream>::write_string(const char* value, func is_end)
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
    template <typename istream, typename T,
        typename std::enable_if<is_nb_signed_integral<T>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return read_int_impl<istream, T>(r.stream); }

    template <typename istream, typename T,
        typename std::enable_if<is_nb_unsigned_integral<T>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return read_uint_impl<istream, T>(r.stream); }

    template <typename istream, typename T,
        typename std::enable_if<std::is_same<T, float>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_float(); }

    template <typename istream, typename T,
        typename std::enable_if<std::is_same<T, double>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_double(); }

    template <typename istream, typename T,
        typename std::enable_if<std::is_same<T, bool>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_bool(); }

    template <typename istream, typename T,
        typename std::enable_if<std::is_same<typename 
        std::remove_cv<T>::type, std::string>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_string(); }

    template <typename istream, typename T,
        typename std::enable_if<std::is_same<T, std::nullptr_t>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { r.read_null(); return nullptr; }

    template <typename ostream, typename T,
        typename std::enable_if<is_nb_signed_integral<T>::value, int>::type = 0>
    static inline void write(raw_writer<ostream>& w, T val) { write_int(w.stream, val); }

    template <typename ostream, typename T,
        typename std::enable_if<is_nb_unsigned_integral<T>::value, int>::type = 0>
    static inline void write(raw_writer<ostream>& w, T val) { write_uint(w.stream, val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, float val) { w.write_float(val); }

    template <typename ostream>
    static inline void write(raw_writer<ostream>& w, double val) { w.write_double(val); }

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
value_type raw_reader<istream>::read(void)
{
    static_assert(!internal::is_cv_char_ptr<value_type>::value,
        "Templated read does not support C-strings. "
        "Use std::string or call read_cstring_unsafe() instead.");
    return internal::rw::read<istream, value_type>(*this);
}

template <typename ostream> 
template <typename value_type> 
void raw_writer<ostream>::write(const value_type& value) 
{
    internal::rw::write(*this, 
        (internal::decay_array_t<const value_type&>)value);
}
}

#endif
