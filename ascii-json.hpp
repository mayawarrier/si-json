
#ifndef ASCII_JSON_HPP
#define ASCII_JSON_HPP

#include <cstddef>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdint>
#include <cinttypes>
#include <cerrno>
#include <cstdlib>
#include <limits>
#include <string>
#include <sstream>
#include <algorithm> // std::copy
#include <memory>
#include <stdexcept>

namespace json {

using intl32_t = std::int_least32_t;
using intl64_t = std::int_least64_t;
using uintl32_t = std::uint_least32_t;
using uintl64_t = std::uint_least64_t;

// Input file stream.
class ifstream
{
private:
    std::FILE* file;
    std::string fpath;
    const std::size_t bufsize;
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
            std::size_t nread = std::fread(buf.get(), sizeof(char), bufsize, file);
            if (std::ferror(file))
                throw std::runtime_error("Failed to read from " + fpath);
            
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
        buf(new char[bufsize]), bufsize(bufsize),
        buf_last_read(buf.get()), buf_eof(false),
        current(buf.get()), posn(0), fpath(filepath)
    {
        file = std::fopen(fpath.c_str(), "r");
        if (!file) throw std::runtime_error("Could not open " + fpath);
    }
    virtual ~ifstream(void) { std::fclose(file); }

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
    std::FILE* file;
    std::string fpath;
    const std::size_t bufsize;
    std::unique_ptr<char[]> buf;
    char* current;
    bool io_error;

public:
    ofstream(const char filepath[], std::size_t bufsize) :
        buf(new char[bufsize]), bufsize(bufsize),
        current(buf.get()), fpath(filepath), io_error(false)
    {
        assert(bufsize > 0);
        file = std::fopen(fpath.c_str(), "w");
        if (!file) throw std::runtime_error("Could not open " + fpath);
    }

    // Flush buffer.
    inline void flush(void)
    {
        const std::size_t towrite = (std::size_t)(current - buf.get());
        std::size_t wrote = std::fwrite(buf.get(), sizeof(char), towrite, file);
        current = buf.get(); // put() must be safe even if we throw
        if (wrote < towrite) {
            io_error = true;
            throw std::runtime_error("Failed to write to " + fpath);
        }      
    }

    // Write a character.
    inline void put(char c) {
        if (current == buf.get() + bufsize) flush();
        *current = c; current++;
    }

    virtual ~ofstream(void) 
    { 
        if (!io_error) flush();    
        std::fclose(file);
    }  
};

// Input memory stream.
class imstream
{
private:
    const char* const begin;
    const char* const end;
    const char* current;
public:
    imstream(char* src, std::size_t size) :
        begin(src), end(src + size), 
        current(src)
    {}
    virtual ~imstream(void) {}

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
private:
    char* buf;
    std::size_t len, cap;
public:
    omstream(std::size_t init_capacity = 0) :
        buf(new char[init_capacity]),
        cap(init_capacity), len(0)
    {}  

    // Flush buffer.
    inline void flush(void) const noexcept { assert(buf); }
    // Write a character.
    inline void put(char c)
    {
        assert(buf);
        if (len + 1 > cap) {
            std::size_t new_cap = 2 * cap + 1;
            char* new_buf = new char[new_cap];
            std::copy(buf, buf + len, new_buf);
            delete[] buf; buf = new_buf;
            cap = new_cap;
        }
        buf[len++] = c;
    }

    // Get length.
    inline std::size_t length(void) const noexcept { return len; }
    // Get capacity.
    inline std::size_t capacity(void) const noexcept { return cap; }

    // Free unused capacity.
    inline void shrink_to_fit(void)
    {
        assert(buf);
        char* new_buf = new char[len];
        std::copy(buf, buf + len, new_buf);
        delete[] buf; buf = new_buf;
        cap = len;
    }

    // Get buffer.
    inline const char* get_buf(void) const noexcept { return buf; }

    // Release (ownership of) buffer.
    // Stream will no longer be modifiable.
    inline char* release_buf(void) noexcept
    {
        char* rval = buf;
        buf = nullptr;
        return rval;
    }

    virtual ~omstream(void) { if (buf) delete[] buf; }
};

namespace internal
{
inline bool is_any_of(const char* chars, char val) 
{
    for (char c = *chars; c != '\0'; c = *chars++) {
        if (c == val) return true;
    }
    return false;
}
inline bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}
inline std::runtime_error parse_err(std::size_t position, const char* expected)
{
    return std::runtime_error("Parse error at offset " +
        std::to_string(position) + " - expected " + expected);
}

template <typename istream>
inline bool end(istream& stream) { return stream.peek() == '\0'; }

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
    for (char c = *str; c != '\0'; c = *str++) stream.put(c);
}
template <typename ostream>
inline void put_reverse(ostream& stream, const std::string& str) {
    for (auto i = str.length() - 1; i > -1; --i) stream.put(str[i]);
}

template <typename istream>
inline bool read_uint64(istream& stream, uintl64_t out_value)
{
    if (end(stream) || stream.peek() == '-' 
        || !is_digit(stream.peek()))
        return false;

    out_value = 0;
    while (!end(stream) && is_digit(stream.peek()) {
        out_value = 10 * out_value + (stream.take() - '0');
    }
    return true;
}

template <typename istream>
inline bool read_int64(istream& stream, intl64_t& out_value)
{
    bool neg = stream.peek() == '-';
    if (neg) stream.take();
    
    uintl64_t uvalue;
    if (!read_uint64(stream, uvalue)) return false;
    if (neg) {
        if (uvalue > 0x8000000000000000) return false;
    } else if (uvalue > 0x7FFFFFFFFFFFFFFF) return false;

    out_value = uvalue * (neg ? -1 : 1);
    return true;
}

template <typename ostream>
inline void write_uint64(ostream& stream, uintl64_t value)
{
    if (value == 0)
        stream.put('0');
    else {
        std::string svalue;
        while (value != 0) {
            // units first
            auto res = std::imaxdiv(value, 10);
            svalue += '0' + (char)res.rem;
            value = res.quot;
        }
        put_reverse(stream, svalue);
    }
}

template <typename ostream>
inline void write_int64(ostream& stream, intl64_t value)
{
    bool neg == value < 0;
    if (neg) stream.put('-');
    write_uint64(stream, neg ? -value : value);
}

template <typename istream, typename value_type>
inline bool read_floating(istream& stream, 
    // one of strtof(), strtod() or strtold()
    value_type (*converter)(const char*, char**), 
    value_type& out_value)
{
    std::string sval;
    while (!end(stream) && !std::isspace(stream.peek())
        && !is_any_of(",]}", stream.peek())) {
        sval += stream.take();
    }
    if (sval.length() == 0) return false;

    char* v_end;
    const char* v_begin = sval.c_str();
    out_value = converter(v_begin, &v_end);

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

        case '"': return "\"";
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
}

// ASCII JSON reader.
template <typename istream>
class asciireader
{
private:
    istream& stream;
public:
    asciireader(istream& stream) :
        stream(stream)
    {}
    virtual ~asciireader(void) {}

    intl32_t read_int32(void);
    intl64_t read_int64(void);
    uintl32_t read_uint32(void);
    uintl64_t read_uint64(void);

    float read_float(void);
    double read_double(void);
    bool read_bool(void);

    // Returns nullptr or null-terminated string.
    std::unique_ptr<char[]> read_string(void);

    void read_start_object(void) { internal::skip_ws_and_read(stream, '{'); }
    void read_end_object(void) { internal::skip_ws_and_read(stream, '}'); }

    void read_start_array(void) { internal::skip_ws_and_read(stream, '['); }
    void read_end_array(void) { internal::skip_ws_and_read(stream, ']'); }

    void read_kv_separator(void) { internal::skip_ws_and_read(stream, ':'); }
    void read_kvpair_separator(void) { internal::skip_ws_and_read(stream, ','); }
};

// ASCII JSON writer.
template <typename ostream>
class asciiwriter
{
private:
    ostream& stream;
public:
    asciiwriter(ostream& stream) :
        stream(stream)
    {}
    virtual ~asciiwriter(void) {
        try { stream.flush(); }
        catch (...) {}
    }

    void write_int32(intl32_t value) { internal::write_int64(stream, value); }
    void write_int64(intl64_t value) { internal::write_int64(stream, value); }
    void write_uint32(uintl32_t value) { internal::write_uint64(stream, value); }
    void write_uint64(uintl64_t value) { internal::write_uint64(stream, value); }

    void write_float(float value) { internal::write_floating(stream, value); }
    void write_double(double value) { internal::write_floating(stream, value); }
    void write_bool(bool value) { internal::put(stream, value ? "true" : "false"); }

    void write_string(const char* value);
    void write_string(const std::string& value) { return write_string(value.c_str()); }

    void write_start_object(void) { stream.put('{'); }
    void write_end_object(void) { stream.put('}'); }

    void write_start_array(void) { stream.put('['); }
    void write_end_array(void) { stream.put(']'); }

    void write_kv_separator(void) { stream.put(':'); }
    void write_kvpair_separator(void) { stream.put(','); }
};


template <typename istream>
intl32_t asciireader<istream>::read_int32(void)
{
    intl64_t value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_int64(stream, value)) goto fail;
    if (value > 0x7FFFFFFF || value < -0x80000000) goto fail;
    return (intl32_t)value;
fail:
    throw internal::parse_err(startpos, "int32");
}

template <typename istream>
intl64_t asciireader<istream>::read_int64(void)
{
    intl64_t value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_int64(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "int64");
}

template <typename istream>
uintl32_t asciireader<istream>::read_uint32(void)
{
    uintl64_t value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_uint64(stream, value)) goto fail;
    if (value > 0xFFFFFFFF) goto fail;
    return (uintl32_t)value;
fail:
    throw internal::parse_err(startpos, "uint32");
}

template <typename istream>
uintl64_t asciireader<istream>::read_uint64(void)
{
    uintl64_t value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_uint64(stream, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "uint64");
}

template <typename istream>
float asciireader<istream>::read_float(void)
{
    float value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, std::strtof, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "float");
}

template <typename istream>
double asciireader<istream>::read_double(void)
{
    double value; std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
    if (!internal::read_floating(stream, std::strtod, value)) goto fail;
    return value;
fail:
    throw internal::parse_err(startpos, "double");
}

template <typename istream>
bool asciireader<istream>::read_bool(void)
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
std::unique_ptr<char[]> asciireader<istream>::read_string(void)
{
    std::size_t startpos;
    if (!internal::skip_ws(stream, startpos)) goto fail;
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
        omstream sbuilder(4);
        while (!internal::end(stream) && stream.peek() != '"') 
        {
            switch (stream.peek()) 
            {
                case '\\':
                    sbuilder.put(internal::take_unescape(stream));
                    break;
                default: 
                    sbuilder.put(stream.take()); 
                    break;                  
            }
        }
        if (internal::end(stream)) goto fail;
        stream.take(); // close quotes
        
        sbuilder.put('\0');
        sbuilder.shrink_to_fit();
        return { sbuilder.release_buf() };
    }
fail:
    throw internal::parse_err(startpos, "string");
}

template <typename ostream>
void asciiwriter<ostream>::write_string(const char* value)
{
    if (value == nullptr)
        internal::put(stream, "null");
    else 
    {
        stream.put('"');
        for (char c = *value; c != '\0'; c = *value++)
        {
            const char* esc = internal::try_escape(c);
            if (esc) internal::put(stream, esc);
            else stream.put(c);
        }
        stream.put('"');
    }
}

}

#endif
