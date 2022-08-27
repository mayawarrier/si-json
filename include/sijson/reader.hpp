
#ifndef SIJSON_READER_HPP
#define SIJSON_READER_HPP

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <memory>
#include <type_traits>
#include <limits>
#include <ios>
#include <istream>
#include <locale>
#include <string>
#include <utility>
#include <stdexcept>

#include "internal/util.hpp"
#include "internal/buffers.hpp"
#include "internal/impl_rw.hpp"

#include "core.hpp"
#include "number.hpp"
#include "stringstream.hpp"
#include "stream_adapters.hpp"

#include "encoding.hpp"

namespace sijson {

// Low-level JSON reader.
template <encoding Encoding, typename Istream>
class raw_reader
{
private:
    using JsonIstream = sijson_istream_t<Istream>;
    using CharT = typename Istream::char_type;

public:
    //using char_type = typename Istream::char_type;
    //using encoding_char_type = EncCharT;
    using stream_type = JsonIstream;

public:
    raw_reader(Istream& stream) :
        m_stream(stream)
    {}

    // Get next unread token, skipping any whitespace.
    sijson::token token(void);

    inline void read_start_object(void) { read_char(0x7b); }
    inline void read_end_object(void) { read_char(0x7d); }
    inline void read_start_array(void) { read_char(0x5b); }
    inline void read_end_array(void) { read_char(0x5d); }
    inline void read_key_separator(void) { read_char(0x3a); }
    inline void read_item_separator(void) { read_char(0x2c); }

    inline std::int_least32_t read_int32(void) 
    { return read_intg<std::int_least32_t, iutil::int32_min, iutil::int32_max>("int32"); }

    inline std::int_least64_t read_int64(void) 
    { return read_intg<std::int_least64_t, iutil::int64_min, iutil::int64_max>("int64"); }

    inline std::uint_least32_t read_uint32(void) 
    { return read_uintg<std::uint_least32_t, iutil::uint32_max>("uint32"); }

    inline std::uint_least64_t read_uint64(void) 
    { return read_uintg<std::uint_least64_t, iutil::uint64_max>("uint64"); }

    inline float read_float(void) { return read_floating<float>("float"); }

    inline double read_double(void) { return read_floating<double>("double"); }

    // Read numerical value.
    inline number read_number(void);

    inline bool read_bool(void);

    inline void read_null(void);

    // Read string. String is unescaped.
    inline std::string read_string(void)
    {
        return read_string<std::allocator<char>>();
    }

    // Read string with custom allocator. String is unescaped.
    template <
        typename StrAllocator = std::allocator<char>>
        inline std::basic_string<char, std::char_traits<char>, StrAllocator> read_string(void)
    {
        basic_ostdstrstream<char, StrAllocator> os;
        read_string_impl(m_stream, os);
        return std::move(os).str();
    }

    // Read string of custom type.
    // DestString must be a std::basic_string.
    // String is unescaped.
    template <typename DestString>
    inline DestString read_string_as(void)
    {
        static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
            "DestString is not a std::basic_string.");

        return read_string<typename DestString::allocator_type>();
    }

    // Read string into output stream.
    // String is unescaped.
    // If quoted is false, quotes are not
    // treated as string delimiters.
    template <typename Ostream>
    inline void read_string_into(Ostream& os, bool quoted = true);

    // Read string into output stream, or read null.
    // If token is string, calls get_os(), writes the string to it, and returns
    // TOKEN_string. If token is null, reads it and returns TOKEN_null.
    // String is unescaped.
    template <typename Func>
    inline sijson::token read_string_or_null(Func get_os)
    {
        return read_string_or_null_impl(m_stream, get_os) ? TOKEN_string : TOKEN_null;
    }

    // Read value of type T.
    // 
    // Supports all types with named read functions in this class
    // (read_float(), read_double(), etc.), and all remaining integral types.
    // Char types (e.g. char16_t, char32_t etc.) are read as integers.
    //
    template <typename T>
    inline T read(void)
    {
        return read_t_impl::template read<T>(*this);
    }

    // Get stream.
    inline stream_type& stream(void) noexcept { return m_stream; }

private:
    template <typename Ostream>
    static inline void take_numstr(JsonIstream& is, Ostream& os);

    template <typename UintT>
    static inline bool read_uintg_impl(JsonIstream& stream, UintT& out_value);
    template <typename IntT, IntT Lbound, IntT Ubound>
    static inline bool read_intg_impl(JsonIstream& stream, IntT& out_value);

    template <bool CheckNanInfHex = true, typename Osstream, typename FloatT>
    static inline bool read_floating_impl(Osstream& numstr, FloatT& out_value);
    static inline bool read_number_impl(JsonIstream& stream, number& out_value);

    template <typename Ostream>
    static inline void take_unescape(JsonIstream& is, Ostream& os);

    template <typename Ostream>
    static inline void read_string_impl(JsonIstream& is, Ostream& os);
    template <typename Func>
    static inline bool read_string_or_null_impl(JsonIstream& is, Func get_os);

    template <typename IntT, 
        IntT Lbound = std::numeric_limits<IntT>::min(), 
        IntT Ubound = std::numeric_limits<IntT>::max()>
    inline IntT read_intg(const char* type_name = nullptr);

    template <typename UintT, 
        UintT Ubound = std::numeric_limits<UintT>::max()>
    inline UintT read_uintg(const char* type_name = nullptr);

    template <typename FloatT>
    inline FloatT read_floating(const char* type_label);

    inline void read_char(char expected);

    struct read_t_impl
    {
        template <typename T, iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value, int> = 0>
        static inline T read(raw_reader& r) { return r.read_intg<T>(); }

        template <typename T, iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value, int> = 0>
        static inline T read(raw_reader& r) { return r.read_uintg<T>(); }

        template <typename T, iutil::enable_if_same_t<T, float> = 0>
        static inline T read(raw_reader& r) { return r.read_float(); }

        template <typename T, iutil::enable_if_same_t<T, double> = 0>
        static inline T read(raw_reader& r) { return r.read_double(); }

        template <typename T, iutil::enable_if_same_t<T, number> = 0>
        static inline T read(raw_reader& r) { return r.read_number(); }

        template <typename T, iutil::enable_if_same_t<T, bool> = 0>
        static inline T read(raw_reader& r) { return r.read_bool(); }

        template <typename T, iutil::enable_if_t<iutil::is_instance_of_basic_string<T, char>::value> = 0>
        static inline T read(raw_reader& r) { return r.read_string(); }

        template <typename T, iutil::enable_if_same_t<T, std::nullptr_t> = 0>
        static inline T read(raw_reader& r) { r.read_null(); return nullptr; }
    };

private:
    sijson_istream_t<Istream&> m_stream;
};

// for now
template <typename Istream>
using raw_ascii_reader = raw_reader<ENCODING_utf8, Istream>;


// ASCII JSON reader.
template <typename Istream,
    // Allocator type used for internal purposes/book-keeping.
    typename Allocator = std::allocator<void>>
class ascii_reader : public internal::rw_base<Allocator>
{
public:
    ascii_reader(Istream& stream) :
        m_rr{ stream }
    {
        this->m_nodes.push({ DOCNODE_root });
    }

    // Get next unread token, skipping any whitespace.
    inline sijson::token token(void) { return m_rr.token(); }

    // Get parent node.
    // For eg. if you call start_object(), parent_node()
    // returns DOCNODE_object until the next call to 
    // end_object() or start_array().
    inline doc_node_type parent_node(void) const { return this->m_nodes.top().type; }

    // Start reading object.
    inline void start_object(void)
    {
        this->template start_node<DOCNODE_object>([&] {
            read_separator();
            m_rr.read_start_object();
        });
    }

    // Start reading array.
    inline void start_array(void)
    {
        this->template start_node<DOCNODE_array>([&] {
            read_separator();
            m_rr.read_start_array();
        });
    }

    // End reading object.
    inline void end_object(void)
    {
        this->template end_node<DOCNODE_object>([&] 
        { m_rr.read_end_object(); });
    }

    // End reading array.
    inline void end_array(void)
    {
        this->template end_node<DOCNODE_array>([&] 
        { m_rr.read_end_array(); });
    }

    // Read object key. String is unescaped.
    inline std::string read_key(void)
    {
        return read_key<std::allocator<char>>();
    }

    // Read object key with custom allocator.
    // String is unescaped.
    template <
        typename StrAllocator = std::allocator<char>>
        inline std::basic_string<char, std::char_traits<char>, StrAllocator> read_key(void);

    // Read object key of custom type.
    // DestString must be a std::basic_string.
    template <typename DestString>
    inline DestString read_key_as(void)
    {
        static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
            "DestString is not a std::basic_string.");

        return read_key<typename DestString::allocator_type>();
    }

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename ...Ts>
    void read_key(const std::basic_string<char, Ts...>& expected_key);

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    void read_key(const char* expected_key);

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    void read_key(const char* expected_key, std::size_t length);

    // Read value.
    template <typename Value>
    Value read_value(void);

    // Read value.
    template <typename Value>
    inline void read_value(Value& out_value)
    {
        out_value = read_value<Value>();
    }

    // Read object key-value pair.
    // Key must be a std::basic_string.
    template <typename Key, typename Value>
    inline std::pair<Key, Value> read_key_value(void);

    // Read object key-value pair.
    template <typename Value>
    inline std::pair<std::string, Value> read_key_value(void)
    {
        return read_key_value<std::string, Value>();
    }

    // Read object key-value pair.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Value, typename ...Ts>
    inline void read_key_value(const std::basic_string<char, Ts...>& expected_key, Value& out_value)
    {
        read_key(expected_key);
        read_value(out_value);
    }

    // Read object key-value pair.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Value>
    inline void read_key_value(const char* expected_key, Value& out_value)
    {
        read_key(expected_key);
        read_value(out_value);
    }

    // Read object key-value pair.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Value>
    inline void read_key_value(const char* expected_key, std::size_t key_length, Value& out_value)
    {
        read_key(expected_key, key_length);
        read_value(out_value);
    }

    // Get stream position.
    inline std::size_t inpos(void) { return m_rr.stream().inpos(); }

    // True if reached end of stream.
    inline bool end(void) { return m_rr.stream().end(); }

private:
    inline void read_separator(void);

    template <typename IsEndpFunc>
    inline bool read_key_impl(const char* str, IsEndpFunc is_endp, std::size_t& out_pos);

private:
    raw_ascii_reader<Istream> m_rr;
};



template <encoding Encoding, typename Istream>
inline void raw_reader<Encoding, Istream>::read_char(char expected)
{
    if (!iutil::skip_ws(m_stream)) goto fail;
    if (m_stream.peek() != expected) goto fail;
    m_stream.take();
    return;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(),
        std::string("'") + expected + '\'');
}

// Get type of token to be read.
template <encoding Encoding, typename Istream>
inline sijson::token raw_reader<Encoding, Istream>::token(void)
{
    if (!iutil::skip_ws(m_stream))
        return TOKEN_eof;

    switch (m_stream.peek())
    {
        case 0x7b: return TOKEN_begin_object;   // '{'
        case 0x7d: return TOKEN_end_object;     // '}'
        case 0x5b: return TOKEN_begin_array;    // '['
        case 0x5d: return TOKEN_end_array;      // ']'
        case 0x3a: return TOKEN_key_separator;  // ':'
        case 0x2c: return TOKEN_item_separator; // ','
        case 0x22: return TOKEN_string;         // '"'
        case 0x74:                              // 't'
        case 0x66: return TOKEN_boolean;        // 'f'
        case 0x6e: return TOKEN_null;           // 'n'
        case 0x2d: return TOKEN_number;         // '-'
        default:
            if (iutil::is_digit(m_stream.peek()))
                return TOKEN_number;
            else
                throw iutil::parse_error_exp(m_stream.inpos(), "token");
    }  
}

template <encoding Encoding, typename Istream>
template <typename UintT>
inline bool raw_reader<Encoding, Istream>::read_uintg_impl(JsonIstream& stream, UintT& out_value)
{
    if (stream.end() || !iutil::is_digit(stream.peek()) || 
        stream.peek() == 0x2d) // '-'
        return false;

    out_value = 0;
    while (!stream.end() && iutil::is_digit(stream.peek()))
    {
        UintT old = out_value;
        out_value = 10 * out_value + (UintT)(stream.peek() - 0x30); // '0'
        if (out_value < old) return false; // overflow
        stream.take();
    }
    return true;
}

template <encoding Encoding, typename Istream>
template <typename IntT, IntT Lbound, IntT Ubound>
inline bool raw_reader<Encoding, Istream>::read_intg_impl(JsonIstream& stream, IntT& out_value)
{
    bool neg = stream.peek() == 0x2d; // '-'
    if (neg) stream.take();

    typename std::make_unsigned<IntT>::type uvalue;
    if (!read_uintg_impl(stream, uvalue)) return false;
    if (neg) {
        if (uvalue > iutil::absu(Lbound)) return false;
    } else if (uvalue > iutil::absu(Ubound)) return false;

    out_value = neg ? iutil::uneg<IntT, Lbound, Ubound>(uvalue) : (IntT)uvalue;
    return true;
}

template <encoding Encoding, typename Istream>
template <typename IntT, IntT Lbound, IntT Ubound>
inline IntT raw_reader<Encoding, Istream>::read_intg(const char* type_name)
{
    IntT value;
    if (!iutil::skip_ws(m_stream)) goto fail;
    if (!read_intg_impl<IntT, Lbound, Ubound>(m_stream, value)) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(), type_name ? type_name : "signed integral type");
}

template <encoding Encoding, typename Istream>
template <typename UintT, UintT Ubound>
inline UintT raw_reader<Encoding, Istream>::read_uintg(const char* type_name)
{
    UintT value;
    if (!iutil::skip_ws(m_stream)) goto fail;
    if (!read_uintg_impl(m_stream, value)) goto fail;
    if (value > Ubound) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(), type_name ? type_name : "unsigned integral type");
}

template <encoding Encoding, typename Istream>
template <typename Ostream>
inline void raw_reader<Encoding, Istream>::take_numstr(JsonIstream& is, Ostream& os)
{
    while (!is.end() && !iutil::is_ws(is.peek()))
    {
        switch (is.peek())
        {
            case 0x2c: case 0x5d: case 0x7d: // ',', ']', '}'
                return;
            default: os.put(is.take());
                break;
        }
    }
}

template <encoding Encoding, typename Istream>
template <bool CheckNanInfHex, typename Osstream, typename FloatT>
inline bool raw_reader<Encoding, Istream>::read_floating_impl(Osstream& numstream, FloatT& out_value)
{
    if (numstream.outpos() == 0)
        return false;

    auto strdata = sijson::outdata(numstream);
    auto* str_begin = strdata.begin;
    auto* str_end = strdata.end;

    bool neg = false;
    if (CheckNanInfHex)
    {
        // skip '-'
        if (*str_begin == 0x2d) {
            neg = true; str_begin++;
            // only sign?
            if (numstream.outpos() == 1)
                return false;
        }

        auto ci_equal = [](CharT lhs, CharT rhs) {
            return rhs == iutil::to_lower(lhs);
        };

        if (iutil::starts_with(str_begin, str_end, "0x", 2, ci_equal) ||
            iutil::starts_with(str_begin, str_end, "nan", 3, ci_equal) ||
            iutil::starts_with(str_begin, str_end, "inf", 3, ci_equal))
            return false;
    }

    internal::memspanbuf streambuf(str_begin,
        (std::streamsize)(str_end - str_begin), std::ios_base::in);
    std::istream sstream(&streambuf);
    sstream.imbue(std::locale::classic()); // make decimal point '.'
    sstream.precision(std::numeric_limits<FloatT>::max_digits10);
    sstream >> out_value;

    if (CheckNanInfHex && neg)
        out_value *= (FloatT)(-1);

    return !sstream.fail();
}

template <encoding Encoding, typename Istream>
template <typename FloatT>
inline FloatT raw_reader<Encoding, Istream>::read_floating(const char* type_label)
{
    std::size_t error_offset;
    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

    FloatT value;
    {
        char strbuf[iutil::max_chars10<FloatT>::value];
        ostrspanstream numstream(strbuf, sizeof(strbuf));
        take_numstr(m_stream, numstream);

        if (!read_floating_impl(numstream, value)) goto fail;
    }
    return value;
fail:
    throw iutil::parse_error_exp(error_offset, type_label);
}

template <encoding Encoding, typename Istream>
inline bool raw_reader<Encoding, Istream>::read_number_impl(JsonIstream& stream, number& out_value)
{
    auto out_inumber = [&](std::uintmax_t value, bool neg) -> bool 
    {
        if (neg) {
            if (value > iutil::absu(std::numeric_limits<std::intmax_t>::min()))
                return false;
            out_value = iutil::uneg<std::intmax_t>(value);
        } 
        else out_value = value;

        return true;
    };

    auto to_fnumber = [](double value) -> number
    {
        // todo: with -ffastmath/fp:fast, this may always select float over double
        if (value > std::numeric_limits<float>::max() || value != (float)value)
            return { value };
        else return { (float)value };
    };

    char fpstrbuf[iutil::max_chars10<double>::value];
    unchecked_ostrspanstream fpstream(fpstrbuf, sizeof(fpstrbuf)); // floating point value

    bool neg = stream.peek() == 0x2d; // '-'
    if (neg) fpstream.put(stream.take());

    if (stream.end() || !iutil::is_digit(stream.peek()))
        return false;

    std::uintmax_t old_int_v, int_v = 0;
    while (!stream.end() && iutil::is_digit(stream.peek()))
    {
        old_int_v = int_v;
        auto c = stream.take(); fpstream.put(c);
        int_v = 10 * int_v + (c - 0x30); // '0'
        if (int_v < old_int_v) return false; // overflow
    }
    if (stream.end() || iutil::is_ws(stream.peek()))
        return out_inumber(int_v, neg);

    switch (stream.peek())
    {
        case 0x2c: case 0x5d: case 0x7d: // ',', ']', '}'
            return out_inumber(int_v, neg);

        case 0x2e: case 0x65: case 0x45: // '.', 'e', 'E'
        {
            double float_v;
            take_numstr(stream, fpstream); // remaining
            bool success = read_floating_impl<false>(fpstream, float_v);
            if (success) out_value = to_fnumber(float_v);
            return success;
        }

        default: return false;
    }
}

template <encoding Encoding, typename Istream>
inline number raw_reader<Encoding, Istream>::read_number(void)
{
    number value; 
    std::size_t error_offset;

    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;
    if (!read_number_impl(m_stream, value)) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(error_offset, "number");
}

template <encoding Encoding, typename Istream>
inline bool raw_reader<Encoding, Istream>::read_bool(void)
{
    std::size_t error_offset;
    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

    if (m_stream.peek() == 0x74) // 't'
    {
        m_stream.take();
        if (m_stream.end() || m_stream.take() != 0x72) goto fail; // 'r'
        if (m_stream.end() || m_stream.take() != 0x75) goto fail; // 'u'
        if (m_stream.end() || m_stream.take() != 0x65) goto fail; // 'e'
        return true;
    }
    else if (m_stream.peek() == 0x66) // 'f'
    {
        m_stream.take();
        if (m_stream.end() || m_stream.take() != 0x61) goto fail; // 'a'
        if (m_stream.end() || m_stream.take() != 0x6c) goto fail; // 'l'
        if (m_stream.end() || m_stream.take() != 0x73) goto fail; // 's'
        if (m_stream.end() || m_stream.take() != 0x65) goto fail; // 'e'
        return false;
    }
fail:
    throw iutil::parse_error_exp(error_offset, "bool");
}

template <encoding Encoding, typename Istream>
inline void raw_reader<Encoding, Istream>::read_null(void)
{
    std::size_t error_offset;
    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

    if (m_stream.take() != 0x6e) goto fail; // 'n'
    if (m_stream.end() || m_stream.take() != 0x75) goto fail; // 'u'
    if (m_stream.end() || m_stream.take() != 0x6c) goto fail; // 'l'
    if (m_stream.end() || m_stream.take() != 0x6c) goto fail; // 'l'
    return;
fail:
    throw iutil::parse_error_exp(error_offset, "null");
}

namespace internal
{
template <typename Traits, typename IsEndpFunc>
class streq_ostream
{
public:
    streq_ostream(const char* str, IsEndpFunc is_endp) :
        m_equal(true), m_strp(str), m_is_endp(is_endp)
    {}

    inline void put(char c)
    {
        if (!m_equal) return;
        if (!m_is_endp(m_strp))
        {
            m_equal = Traits::eq(*m_strp, c);
            m_strp++;
        }
        else m_equal = false;
    }

    inline bool str_is_equal(void) const noexcept
    {
        return m_equal && m_is_endp(m_strp);
    }
private:
    bool m_equal;
    const char* m_strp;
    IsEndpFunc m_is_endp;
};
}

template <encoding Encoding, typename Istream>
template <typename Ostream>
inline void raw_reader<Encoding, Istream>::take_unescape(JsonIstream& is, Ostream& os)
{
    assert(!is.end());
    const char* EXSTR_bad_escape = "Invalid escape sequence.";

    if (is.peek() != 0x5c) // '\'
        os.put(is.take());
    else
    {
        is.take(); // take '\'

        if (is.end())
            throw iutil::parse_error(is.inpos(), EXSTR_bad_escape);

        switch (is.take()) // unescape
        {
            case 0x62: os.put(0x08); break; // '\b'
            case 0x66: os.put(0x0c); break; // '\f'
            case 0x6e: os.put(0x0a); break; // '\n'
            case 0x72: os.put(0x0d); break; // '\r'
            case 0x74: os.put(0x09); break; // '\t'
            case 0x22: os.put(0x22); break; // '"'
            case 0x2f: os.put(0x2f); break; // '/'
            default: 
                throw iutil::parse_error(is.inpos() - 1, EXSTR_bad_escape);
        }
    }
}

template <encoding Encoding, typename Istream>
template <typename Ostream>
inline void raw_reader<Encoding, Istream>::read_string_impl(JsonIstream& is, Ostream& os)
{
    if (!iutil::skip_ws(is) || is.peek() != 0x22) // '"'
        goto fail;

    is.take(); // open quotes
    while (!is.end() && is.peek() != 0x22) // '"'
        take_unescape(is, os);

    if (is.end()) goto fail;
    is.take(); // close quotes
    return;

fail:
    throw iutil::parse_error_exp(is.inpos(), "string");
}

template <encoding Encoding, typename Istream>
template <typename Func>
inline bool
raw_reader<Encoding, Istream>::read_string_or_null_impl(JsonIstream& is, Func get_os)
{
    if (!iutil::skip_ws(is)) goto fail;

    if (is.peek() == 0x6e) // 'n'
    {
        is.take();
        if (is.end() || is.take() != 0x75) goto fail; // 'u'
        if (is.end() || is.take() != 0x6c) goto fail; // 'l'
        if (is.end() || is.take() != 0x6c) goto fail; // 'l'
        return false;
    }
    else if (is.peek() == 0x22) // '"'
    {
        auto&& os = get_os();

        is.take(); // open quotes
        while (!is.end() && is.peek() != 0x22) // '"'
            take_unescape(is, os);

        if (is.end()) goto fail;
        is.take(); // close quotes

        return true;
    }
fail:
    throw iutil::parse_error_exp(is.inpos(), "string or null");
}

template <encoding Encoding, typename Istream>
template <typename Ostream>
inline void raw_reader<Encoding, Istream>::read_string_into(Ostream& os, bool quoted)
{
    if (quoted)
        read_string_impl(m_stream, os);
    else {
        // if none of this succeeds, string will just be empty
        iutil::skip_ws(m_stream);
        while (!m_stream.end())
            take_unescape(m_stream, os);
    }
}

template <typename Istream, typename Allocator>
inline void ascii_reader<Istream, Allocator>::read_separator(void)
{
    if (this->m_nodes.top().has_children)
    {
        switch (this->m_nodes.top().type)
        {
            case DOCNODE_object:
            case DOCNODE_array: m_rr.read_item_separator(); break;
            case DOCNODE_root: throw std::runtime_error(this->EXSTR_multi_root);
            default: assert(false);
        }
    }
    // key node is popped before it can have children
    else if (this->m_nodes.top().type == DOCNODE_key)
        m_rr.read_key_separator();
}

template <typename Istream, typename Allocator>
template <typename StrAllocator>
inline std::basic_string<char, std::char_traits<char>, StrAllocator> ascii_reader<Istream, Allocator>::read_key(void)
{
    this->template assert_rule<DOCNODE_key>();

    read_separator();
    auto str = m_rr.template read_string<StrAllocator>();

    this->m_nodes.push({ DOCNODE_key });
    // don't end_child_node(), key-value pair is incomplete
    return str;
}

template <typename Istream, typename Allocator>
template <typename IsEndpFunc>
inline bool ascii_reader<Istream, Allocator>::read_key_impl(const char* str, IsEndpFunc is_endp, std::size_t& out_pos)
{
    this->template assert_rule<DOCNODE_key>();

    read_separator();
    iutil::skip_ws(m_rr.stream(), out_pos);

    internal::streq_ostream<std::char_traits<char>, IsEndpFunc> os(str, is_endp);
    m_rr.read_string_into(os);

    this->m_nodes.push({ DOCNODE_key });
    // don't end_child_node(), key-value pair is incomplete

    return os.str_is_equal();
}

template <typename Istream, typename Allocator>
template <typename ...Ts>
inline void ascii_reader<Istream, Allocator>::read_key(const std::basic_string<char, Ts...>& expected_key)
{
    auto is_endp = [&](const char* p) { return p == expected_key.data() + expected_key.size(); };

    std::size_t startpos;
    if (!read_key_impl(expected_key.data(), is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key.data(), expected_key.length()) + "\"");
}

template <typename Istream, typename Allocator>
inline void ascii_reader<Istream, Allocator>::read_key(const char* expected_key)
{
    auto is_endp = [](const char* p) { return *p == '\0'; };

    std::size_t startpos;
    if (!read_key_impl(expected_key, is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key) + "\"");
}

template <typename Istream, typename Allocator>
inline void ascii_reader<Istream, Allocator>::read_key(const char* expected_key, std::size_t length)
{
    auto is_endp = [&](const char* p) { return p == expected_key + length; };

    std::size_t startpos;
    if (!read_key_impl(expected_key, is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key, length) + "\"");
}

template <typename Istream, typename Allocator>
template <typename Value>
inline Value ascii_reader<Istream, Allocator>::read_value(void)
{
    this->template assert_rule<DOCNODE_value>();

    read_separator();
    Value value = m_rr.template read<Value>();

    this->end_child_node();
    return value;
}

template <typename Istream, typename Allocator>
template <typename Key, typename Value>
inline std::pair<Key, Value> ascii_reader<Istream, Allocator>::read_key_value(void)
{
    static_assert(iutil::is_instance_of_basic_string<Key, char>::value, 
        "Key must be a std::basic_string with value_type char.");

    using KeyAllocator = typename Key::allocator_type;

    this->template assert_rule<DOCNODE_key, DOCNODE_value>();

    if (this->m_nodes.top().has_children)
        m_rr.read_item_separator();

    Key key = m_rr.template read_string<KeyAllocator>();
    m_rr.read_key_separator();
    Value value = m_rr.template read<Value>();

    this->end_child_node();
    return { std::move(key), std::move(value) };
}

}

#endif
