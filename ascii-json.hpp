
#ifndef ASCII_JSON_HPP
#define ASCII_JSON_HPP

#if defined(_MSC_VER) && !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS
#define ASCII_JSON_UNDEF_CRT_SECURE
#endif

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
#include <string>
#include <sstream>
#include <utility>
#include <algorithm> // std::copy
#include <memory>
#include <vector>
#include <stack>
#include <stdexcept>
#include <type_traits>

namespace json {

namespace internal
{
using unique_file_ptr = std::unique_ptr<std::FILE, void(*)(std::FILE*)>;

inline unique_file_ptr make_unique_fptr(const char* filepath, const char* mode)
{
    std::FILE* file = std::fopen(filepath, mode);
    if (!file) throw std::runtime_error(std::string("Could not open ") + filepath);
    return { file, [](std::FILE* fp) { std::fclose(fp); } };
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
            std::size_t nread = std::fread(buf.get(), sizeof(char), bufsize, file.get());
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
        file(internal::make_unique_fptr(filepath, "rb")),
        buf(new char[bufsize]), 
        buf_last_read(buf.get()), buf_eof(false),
        current(buf.get()), posn(SIZE_MAX)
    {
        if (bufsize == 0)
            throw std::logic_error("Buffer size must be > 0");
        // fill buffer
        advance();
    }
    virtual ~ifstream(void) {}

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
        file(internal::make_unique_fptr(filepath, "wb")),
        buf(new char[bufsize]), buf_end(buf.get() + bufsize),
        current(buf.get()), filepath(filepath)
    {
        if (bufsize == 0)
            throw std::logic_error("Buffer size must be > 0");
    }

    ofstream& operator=(const ofstream&) = delete;
    ofstream& operator=(ofstream&&) = default;   

    // Commit as-yet-unwritten data to target.
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
    const char* end;
    const char* current;
public:
    imstream(char* src, std::size_t size) :
        begin(src), end(src + size),
        current(src)
    {}
    virtual ~imstream(void) {}

    imstream& operator=(const imstream&) = delete;
    imstream& operator=(imstream&&) = default;

    // Get current character.
    inline char peek(void) const noexcept { return current == end ? '\0' : *current; }
    // Get current character then advance.
    inline char take(void) noexcept { return current == end ? '\0' : *current++; }
    // Get position.
    inline std::size_t pos(void) const noexcept { return (std::size_t)(current - begin); }
};

// Output memory stream.
class omstream
{
protected:
    std::unique_ptr<char[]> buf;
    std::size_t len, cap;

    void set_cap(std::size_t new_cap)
    {
        if (cap != new_cap)
        {
            assert(buf && new_cap >= len);
            char* new_buf = new char[new_cap];
            std::copy(buf.get(), buf.get() + len, new_buf);
            buf.reset(new_buf);
            cap = new_cap;
        }
    }
public:
    omstream(std::size_t init_capacity = 0) :
        buf(new char[init_capacity]),
        cap(init_capacity), len(0)
    {}

    omstream& operator=(const omstream&) = delete;
    omstream& operator=(omstream&&) = default;

    // Commit as-yet-unwritten data to target.
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

    virtual ~omstream(void) {}
};

namespace internal
{
template <typename T>
struct is_nonbool_integral : std::integral_constant<bool,
    std::is_integral<T>::value && !std::is_same<T, bool>::value>
{};

template <typename T>
struct is_signed_integral : std::integral_constant<bool,
    is_nonbool_integral<T>::value && std::is_signed<T>::value>
{};
template <typename T>
struct is_unsigned_integral : std::integral_constant<bool,
    is_nonbool_integral<T>::value && std::is_unsigned<T>::value>
{};

// Absolute value of a signed integer.
// Safe to use with signed min().
template <typename T, 
    typename uT = typename std::make_unsigned<T>::type, 
    typename = typename std::enable_if<is_signed_integral<T>::value>::type>
uT absu(T value) 
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
inline bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}
inline void trim_front(std::string& str, const char* target)
{
    auto off = str.find(target);
    if (off != std::string::npos)
        str.erase(off, std::strlen(target));
}

template <typename istream>
inline bool end(istream& stream) { return stream.peek() == '\0'; }

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
    while (!end(stream) && std::isspace(stream.peek())) {
        stream.take();
    }
    return !end(stream);
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
    if (end(stream) || !is_digit(stream.peek()) 
        || stream.peek() == '-')
        return false;

    out_value = 0;
    while (!end(stream) && is_digit(stream.peek())) {
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
    while (!end(stream) && !std::isspace(stream.peek())
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
    NODE_document = 1 << 3,
    NODE_NTYPES = 4
};

struct node_t
{
    const node_type type;
    bool has_children;

    node_t(node_type type) : 
        type(type), has_children(false)
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
};

inline std::string node_desc(unsigned node_types)
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
            case NODE_document: desc += ", document"; break;
            default: assert(false);
        }
    }
    trim_front(desc, ", ");
    return desc;
}

void top_node_assert(const std::stack<node_t>& nodes, unsigned allowed_types)
{
    if (!nodes.top().type_is_any_of(allowed_types))
        throw std::logic_error("Expected node: " + node_desc(allowed_types));
}
void pop_node_assert(std::stack<node_t>& nodes, node_type expected)
{
    top_node_assert(nodes, expected);
    nodes.pop();
}

class cstr_builder : public omstream
{
public:
    cstr_builder(std::size_t init_capacity = 0) :
        omstream(init_capacity)
    {}

    cstr_builder& operator=(const cstr_builder&) = delete;
    cstr_builder& operator=(cstr_builder&&) = default;

    // Get null-terminated string.
    inline const char* get(void) {
        put('\0'); len--;
        return buf.get();
    }
    // Release (ownership of) string.
    inline char* release(void) {
        set_cap(len + 1); put('\0');
        return buf.release();
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

    // str_type must be movable and implement
    // reserve() and push_back().
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

    // Read null or null-terminated string.
    // String must be delete[]d after usage.
    inline char* read_cstring(void);
    // Read non-null string.
    inline std::string read_string(void);

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

    inline void write_string(const char* value);
    inline void write_string(const std::string& value) { write_string(value.c_str()); }

    inline void write_start_object(void) { stream.put('{'); }
    inline void write_end_object(void) { stream.put('}'); }
    inline void write_start_array(void) { stream.put('['); }
    inline void write_end_array(void) { stream.put(']'); }
    inline void write_key_separator(void) { stream.put(':'); }
    inline void write_item_separator(void) { stream.put(','); }

    // Write arbitrary value.
    template <typename value_type> 
    inline void write(value_type&& value);

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

public:
    reader(istream& stream) : rrr{ stream }
    { nodes.push({ internal::NODE_document }); }

    inline void start_object(void);
    inline void start_array(void);
    inline void end_object(void);
    inline void end_array(void);

    // Read object key as null-terminated string.
    // String must be delete[]d after usage.
    inline char* read_key_cstr(void);

    // Read object key.
    inline std::string read_key(void);

    // Read value.
    template <typename value_type>
    inline value_type read_value(void);

    // Read object key-value pair.
    template <typename value_type>
    inline std::pair<std::string, value_type> read_key_value(void);

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

public:
    writer(ostream& stream) : rwr{ stream }
    { nodes.push({ internal::NODE_document }); }

    inline void start_object(void);
    inline void start_array(void);
    inline void end_object(void);
    inline void end_array(void);

    // Write object key.
    inline void write_key(const char* key);
    // Write object key.
    inline void write_key(const std::string& key) { return write_key(key.c_str()); }

    // Write value.
    template <typename value_type>
    inline void write_value(value_type&& value);

    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const char* key, value_type&& value);

    // Write object key-value pair.
    template <typename value_type>
    inline void write_key_value(const std::string& key, value_type&& value)
    { write_key_value(key.c_str(), std::forward<value_type>(value)); }

    // Synonym for stream.flush().
    inline void flush(void) { rwr.flush(); }

    virtual ~writer(void) {
        assert(nodes.size() == 1 && nodes.top().type == 
            internal::NODE_document && "Incomplete JSON.");
        try { flush(); } catch (...) {}
    }
};

template <typename ostream>
void writer<ostream>::maybe_write_separator(void)
{
    if (nodes.top().has_children)
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
    if (nodes.top().has_children)
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
    internal::top_node_assert(nodes, ~internal::NODE_object);
    maybe_write_separator();
    rwr.write_start_object();
    nodes.push({ internal::NODE_object });
}
template <typename istream>
void reader<istream>::start_object(void)
{
    internal::top_node_assert(nodes, ~internal::NODE_object);
    maybe_read_separator();
    rrr.read_start_object();
    nodes.push({ internal::NODE_object });
}

template <typename ostream>
void writer<ostream>::start_array(void)
{
    internal::top_node_assert(nodes, ~internal::NODE_object);
    maybe_write_separator();
    rwr.write_start_array();
    nodes.push({ internal::NODE_array });
}
template <typename istream>
void reader<istream>::start_array(void)
{
    internal::top_node_assert(nodes, ~internal::NODE_object);
    maybe_read_separator();
    rrr.read_start_array();
    nodes.push({ internal::NODE_array });
}

template <typename ostream>
void writer<ostream>::end_object(void)
{
    internal::pop_node_assert(nodes, internal::NODE_object); 
    rwr.write_end_object();
}
template <typename istream>
void reader<istream>::end_object(void)
{
    internal::pop_node_assert(nodes, internal::NODE_object);
    rrr.read_end_object();
}

template <typename ostream>
void writer<ostream>::end_array(void)
{
    internal::pop_node_assert(nodes, internal::NODE_array);
    rwr.write_end_array();
}
template <typename istream>
void reader<istream>::end_array(void)
{
    internal::pop_node_assert(nodes, internal::NODE_array);
    rrr.read_end_array();
}

template <typename ostream>
void writer<ostream>::write_key(const char* key)
{
    internal::top_node_assert(nodes, internal::NODE_object);
    if (!key) throw std::logic_error("object key is null");

    maybe_write_separator();
    rwr.write_string(key);

    nodes.top().has_children = true;
    nodes.push({ internal::NODE_key });
}

template <typename istream>
char* reader<istream>::read_key_cstr(void)
{
    internal::top_node_assert(nodes, internal::NODE_object);

    maybe_read_separator();
    rrr.skip_ws();
    std::size_t startpos = rrr.pos();
    char* str = rrr.read_cstring();
    if (str == nullptr) 
        throw internal::parse_err(startpos, "non-null string");

    nodes.top().has_children = true;
    nodes.push({ internal::NODE_key });
    return str;
}

template <typename istream>
std::string reader<istream>::read_key(void)
{
    internal::top_node_assert(nodes, internal::NODE_object);

    maybe_read_separator();
    std::string str = rrr.read_string();

    nodes.top().has_children = true;
    nodes.push({ internal::NODE_key });
    return str;
}

template <typename ostream>
template <typename value_type>
void writer<ostream>::write_value(value_type&& value)
{
    internal::top_node_assert(nodes, ~internal::NODE_object);

    maybe_write_separator();
    rwr.write(std::forward<value_type>(value));

    if (nodes.top().type == internal::NODE_key) nodes.pop();
    else nodes.top().has_children = true;
}

template <typename istream>
template <typename value_type>
value_type reader<istream>::read_value(void)
{
    internal::top_node_assert(nodes, ~internal::NODE_object);

    maybe_read_separator();
    value_type value = rrr.read<value_type>();

    if (nodes.top().type == internal::NODE_key) nodes.pop();
    else nodes.top().has_children = true;

    return value;
}

template <typename ostream>
template <typename value_type>
void writer<ostream>::write_key_value(const char* key, value_type&& value)
{
    internal::top_node_assert(nodes, internal::NODE_object);
    if (!key) throw std::logic_error("object key is null");

    if (nodes.top().has_children) 
        rwr.write_item_separator();
 
    rwr.write_string(key);
    rwr.write_key_separator();
    rwr.write(std::forward<value_type>(value));

    nodes.top().has_children = true;
}

template <typename istream>
template <typename value_type>
std::pair<std::string, value_type> reader<istream>::read_key_value(void)
{
    internal::top_node_assert(nodes, internal::NODE_object);
    if (nodes.top().has_children) rrr.read_item_separator();

    std::pair<std::string, value_type> ret; // NRVO
    ret.first = rrr.read_string();
    rrr.read_key_separator();
    ret.second = rrr.read<value_type>();

    nodes.top().has_children = true;
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
internal::string_or_null<str_type> 
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
        while (!internal::end(stream) && stream.peek() != '"')
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
        if (internal::end(stream)) goto fail;
        stream.take(); // close quotes

        return { std::move(str) };
    }
fail:
    throw internal::parse_err(startpos, "string");
}

template <typename istream>
std::string raw_reader<istream>::read_string(void)
{
    std::size_t startpos;
    auto maybe_string = read_string_variant<std::string>(&startpos);
    if (maybe_string.is_null())
        throw internal::parse_err(startpos, "non-null string");
    return maybe_string.move_string();
}

template <typename istream>
char* raw_reader<istream>::read_cstring(void)
{
    auto maybe_string = read_string_variant<internal::cstr_builder>();
    return maybe_string.is_string() ?
        maybe_string.get_string().release() : nullptr;
}

template <typename ostream>
void raw_writer<ostream>::write_string(const char* value)
{
    if (value == nullptr)
        internal::put(stream, "null");
    else
    {
        stream.put('"');
        for (char c = *value++; c != '\0'; c = *value++)
        {
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
        typename std::enable_if<is_signed_integral<T>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return read_int_impl<istream, T>(r.stream); }

    template <typename istream, typename T,
        typename std::enable_if<is_unsigned_integral<T>::value, int>::type = 0>
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

    // no overload for char* as it might leak. Use reader.read_cstring() instead.

    template <typename istream, typename T,
        typename std::enable_if<
        std::is_same<typename std::decay<T>::type, std::string>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { return r.read_string(); }

    template <typename istream, typename T,
        typename std::enable_if<std::is_same<T, std::nullptr_t>::value, int>::type = 0>
    static inline T read(raw_reader<istream>& r) { r.read_null(); return nullptr; }

    template <typename ostream, typename T,
        typename std::enable_if<is_signed_integral<T>::value, int>::type = 0>
    static inline void write(raw_writer<ostream>& w, T val) { write_int(w.stream, val); }

    template <typename ostream, typename T,
        typename std::enable_if<is_unsigned_integral<T>::value, int>::type = 0>
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
    return internal::rw::read<istream, value_type>(*this);
}

template <typename ostream> 
template <typename value_type> 
void raw_writer<ostream>::write(value_type&& value) 
{
    internal::rw::write(*this, std::forward<value_type>(value)); 
}
}

#ifdef ASCII_JSON_UNDEF_CRT_SECURE
#undef _CRT_SECURE_NO_WARNINGS
#undef ASCII_JSON_UNDEF_CRT_SECURE
#endif

#endif
