
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

#include "common.hpp"
#include "number.hpp"
#include "stringstream.hpp"
#include "stdstream.hpp"


namespace sijson {

// Low-level ASCII JSON reader.
template <typename Istream>
class raw_ascii_reader
{
private:
    using JsonIstream = wrap_std_istream_t<Istream>;

public:
    using stream_type = JsonIstream;

public:
    raw_ascii_reader(Istream& stream) :
        m_stream(stream)
    {}

    // Get next unread token, skipping any whitespace.
    token_t token(void);

    inline void read_start_object(void) { read_char('{'); }
    inline void read_end_object(void) { read_char('}'); }
    inline void read_start_array(void) { read_char('['); }
    inline void read_end_array(void) { read_char(']'); }
    inline void read_key_separator(void) { read_char(':'); }
    inline void read_item_separator(void) { read_char(','); }

    inline std::int_least32_t read_int32(void) { return read_int_lst<std::int_least32_t>("int32"); }
    inline std::int_least64_t read_int64(void) { return read_int_lst<std::int_least64_t>("int64"); }
    inline std::uint_least32_t read_uint32(void) { return read_uint_lst<std::uint_least32_t>("uint32"); }
    inline std::uint_least64_t read_uint64(void) { return read_uint_lst<std::uint_least64_t>("uint64"); }

    inline float read_float(void) { return read_floating<float>("float"); }
    inline double read_double(void) { return read_floating<double>("double"); }

    // Read numerical value.
    inline number read_number(void);

    inline bool read_bool(void);

    inline void read_null(void);

    // Read string. String is unescaped.
    inline std::string read_string(void)
    {
        return read_string<std::char_traits<char>, std::allocator<char>>();
    }

    // Read string with custom traits/allocator. String is unescaped.
    template <
        typename Traits,
        typename Allocator = std::allocator<char>>
        inline std::basic_string<char, Traits, Allocator> read_string(void)
    {
        basic_ostdsstream<Traits, Allocator> os;
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

        return read_string<typename DestString::traits_type, typename DestString::allocator_type>();
    }

    // Read string into output stream.
    // String is unescaped.
    // If quoted is false, quotes are not
    // treated as string delimiters.
    template <typename Ostream>
    inline void read_string(Ostream& os, bool quoted = true);

    // Read string into output stream, or read null.
    // If token is string, calls get_os(), writes the string to it, and returns
    // TOKEN_string. If token is null, reads it and returns TOKEN_null.
    // String is unescaped.
    template <typename Func>
    inline token_t read_string_or_null(Func get_os)
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
    template <typename IntT, IntT lbound, IntT ubound>
    static inline bool read_intg_impl(JsonIstream& stream, IntT& out_value);
    template <typename IntT>
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

    template <typename IntT>
    inline IntT read_intg_t(void);
    template <typename UintT>
    inline UintT read_uintg_t(void);

    template <typename IntLeastT>
    inline IntLeastT read_int_lst(const char* type_label);
    template <typename UintLeastT>
    inline UintLeastT read_uint_lst(const char* type_label);
    template <typename FloatT>
    inline FloatT read_floating(const char* type_label);

    inline void read_char(char expected);

    struct read_t_impl
    {
        template <typename T, iutil::require_t<iutil::is_nb_signed_integral<T>::value> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_intg_t<T>(); }

        template <typename T, iutil::require_t<iutil::is_nb_unsigned_integral<T>::value> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_uintg_t<T>(); }

        template <typename T, iutil::require_same_t<T, float> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_float(); }

        template <typename T, iutil::require_same_t<T, double> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_double(); }

        template <typename T, iutil::require_same_t<T, number> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_number(); }

        template <typename T, iutil::require_same_t<T, bool> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_bool(); }

        template <typename T, iutil::require_t<iutil::is_instance_of_basic_string<T, char>::value> = 0>
        static inline T read(raw_ascii_reader& r) { return r.read_string(); }

        template <typename T, iutil::require_same_t<T, std::nullptr_t> = 0>
        static inline T read(raw_ascii_reader& r) { r.read_null(); return nullptr; }
    };

private:
    wrap_std_istream_t<Istream&> m_stream;
};



// ASCII JSON reader.
template <typename Istream,
    // Allocator type used for internal purposes/book-keeping.
    typename AllocatorPolicy = std::allocator<void>>
class ascii_reader : public internal::rw_base<AllocatorPolicy>
{
public:
    ascii_reader(Istream& stream) :
        m_rr{ stream }
    {
        this->m_nodes.push({ DOCNODE_root });
    }

    // Get next unread token, skipping any whitespace.
    inline token_t token(void) { return m_rr.token(); }

    // Get parent node.
    // For eg. if you call start_object(), parent_node()
    // returns DOCNODE_object until the next call to 
    // end_object() or start_array().
    inline doc_node_t parent_node(void) const { return this->m_nodes.top().type; }

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
        return read_key<std::char_traits<char>, std::allocator<char>>();
    }

    // Read object key with custom traits/allocator.
    // String is unescaped.
    template <
        typename Traits,
        typename Allocator = std::allocator<char>>
        inline std::basic_string<char, Traits, Allocator> read_key(void);

    // Read object key of custom type.
    // DestString must be a std::basic_string.
    template <typename DestString>
    inline DestString read_key_as(void)
    {
        static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
            "DestString is not a std::basic_string.");

        return read_key<typename DestString::traits_type, typename DestString::allocator_type>();
    }

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Traits, typename Allocator>
    void read_key(const std::basic_string<char, Traits, Allocator>& expected_key);

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
    template <typename KeyTraits, typename KeyAllocator, typename Value>
    inline void read_key_value(const std::basic_string<char, KeyTraits, KeyAllocator>& expected_key, Value& out_value)
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

    template <typename Traits, typename IsEndpFunc>
    inline bool read_key_impl(const char* str, IsEndpFunc is_endp, std::size_t& out_pos);

private:
    raw_ascii_reader<Istream> m_rr;
};



template <typename Istream>
inline void raw_ascii_reader<Istream>::read_char(char expected)
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
template <typename Istream>
inline token_t raw_ascii_reader<Istream>::token(void)
{
    if (!iutil::skip_ws(m_stream))
        return TOKEN_eof;

    switch (m_stream.peek())
    {
        case '{': return TOKEN_begin_object;
        case '}': return TOKEN_end_object;
        case '[': return TOKEN_begin_array;
        case ']': return TOKEN_end_array;
        case ':': return TOKEN_key_separator;
        case ',': return TOKEN_item_separator;
        case '"': return TOKEN_string;
        case 't':
        case 'f': return TOKEN_boolean;
        case 'n': return TOKEN_null;
        case '-': return TOKEN_number;
        default:
            if (iutil::is_digit(m_stream.peek()))
                return TOKEN_number;
            break;
    }
    throw iutil::parse_error_exp(m_stream.inpos(), "token");
}

template <typename Istream>
template <typename UintT>
inline bool raw_ascii_reader<Istream>::read_uintg_impl(JsonIstream& stream, UintT& out_value)
{
    if (stream.end() || !iutil::is_digit(stream.peek())
        || stream.peek() == '-')
        return false;

    out_value = 0;
    while (!stream.end() && iutil::is_digit(stream.peek())) 
    {
        UintT old = out_value;
        out_value = 10 * out_value + (stream.peek() - '0');
        if (out_value < old) return false; // overflow
        stream.take();
    }
    return true;
}

template <typename Istream>
template <typename IntT, IntT lbound, IntT ubound>
inline bool raw_ascii_reader<Istream>::read_intg_impl(JsonIstream& stream, IntT& out_value)
{
    bool neg = stream.peek() == '-';
    if (neg) stream.take();

    typename std::make_unsigned<IntT>::type uvalue;
    if (!read_uintg_impl(stream, uvalue)) return false;
    if (neg) {
        if (uvalue > iutil::absu(lbound)) return false;
    } else if (uvalue > iutil::absu(ubound)) return false;

    out_value = neg ? iutil::uneg<IntT, lbound, ubound>(uvalue) : (IntT)uvalue;
    return true;
}

template <typename Istream>
template <typename IntT>
inline bool raw_ascii_reader<Istream>::read_intg_impl(JsonIstream& stream, IntT& out_value)
{
    return read_intg_impl<IntT,
        std::numeric_limits<IntT>::min(),
        std::numeric_limits<IntT>::max()>(stream, out_value);
}

template <typename Istream>
template <typename IntT>
inline IntT raw_ascii_reader<Istream>::read_intg_t(void)
{
    IntT value;
    if (!iutil::skip_ws(m_stream)) goto fail;
    if (!read_intg_impl(m_stream, value)) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(), "signed integral type");
}

template <typename Istream>
template <typename UintT>
inline UintT raw_ascii_reader<Istream>::read_uintg_t(void)
{
    UintT value;
    if (!iutil::skip_ws(m_stream)) goto fail;
    if (!read_uintg_impl(m_stream, value)) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(), "unsigned integral type");
}

template <typename Istream>
template <typename IntLeastT>
inline IntLeastT raw_ascii_reader<Istream>::read_int_lst(const char* type_label)
{   
    if (!iutil::skip_ws(m_stream)) goto fail;

    IntLeastT value;
    if (!read_intg_impl<IntLeastT,
        iutil::least_t_exp_min<IntLeastT>::value,
        iutil::least_t_exp_max<IntLeastT>::value>(m_stream, value)) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(), type_label);
}

template <typename Istream>
template <typename UintLeastT>
inline UintLeastT raw_ascii_reader<Istream>::read_uint_lst(const char* type_label)
{
    if (!iutil::skip_ws(m_stream)) goto fail;

    UintLeastT value;      
    if (!read_uintg_impl(m_stream, value)) goto fail;
    if (value > iutil::least_t_exp_max<UintLeastT>::value) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(m_stream.inpos(), type_label);
}

template <typename Istream>
template <typename Ostream>
inline void raw_ascii_reader<Istream>::take_numstr(JsonIstream& is, Ostream& os)
{
    while (!is.end() && !iutil::is_ws(is.peek()))
    {
        switch (is.peek())
        {
            case ',': case']': case '}':
                return;
            default: os.put(is.take());
                break;
        }
    }
}

template <typename Istream>
template <bool CheckNanInfHex, typename Osstream, typename FloatT>
inline bool raw_ascii_reader<Istream>::read_floating_impl(Osstream& numstream, FloatT& out_value)
{
    if (numstream.outpos() == 0)
        return false;

    auto strdata = numstream.outdata();
    auto str_begin = strdata.begin;
    auto str_end = strdata.end;

    bool neg = false;
    if (CheckNanInfHex)
    {
        // skip '-'
        if (*str_begin == '-') {
            neg = true; str_begin++;
            if (numstream.outpos() == 1)
                return false;
        }

        auto is_ci_equal = [](char lhs, char rhs) {
            return rhs == iutil::to_lower(lhs);
        };
        if (iutil::starts_with(str_begin, str_end, "0x", is_ci_equal) ||
            iutil::starts_with(str_begin, str_end, "nan", is_ci_equal) ||
            iutil::starts_with(str_begin, str_end, "inf", is_ci_equal))
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

template <typename Istream>
template <typename FloatT>
inline FloatT raw_ascii_reader<Istream>::read_floating(const char* type_label)
{
    std::size_t error_offset;
    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

    FloatT value;
    {
        char strbuf[iutil::max_chars10<FloatT>::value];
        unchecked_ostrspanstream numstream(strbuf, sizeof(strbuf));
        take_numstr(m_stream, numstream);

        if (!read_floating_impl(numstream, value)) goto fail;
    }
    return value;
fail:
    throw iutil::parse_error_exp(error_offset, type_label);
}

template <typename Istream>
inline bool raw_ascii_reader<Istream>::read_number_impl(JsonIstream& stream, number& out_value)
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

    bool neg = stream.peek() == '-';
    if (neg) fpstream.put(stream.take());

    if (stream.end() || !iutil::is_digit(stream.peek()))
        return false;

    std::uintmax_t old_int_v, int_v = 0;
    while (!stream.end() && iutil::is_digit(stream.peek()))
    {
        old_int_v = int_v;
        char c = stream.take(); fpstream.put(c);
        int_v = 10 * int_v + (c - '0');
        if (int_v < old_int_v) return false; // overflow
    }
    if (stream.end() || iutil::is_ws(stream.peek()))
        return out_inumber(int_v, neg);

    switch (stream.peek())
    {
        case ',': case']': case '}':
            return out_inumber(int_v, neg);

        case '.': case 'e': case 'E':
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

template <typename Istream>
inline number raw_ascii_reader<Istream>::read_number(void)
{
    number value; 
    std::size_t error_offset;

    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;
    if (!read_number_impl(m_stream, value)) goto fail;
    return value;
fail:
    throw iutil::parse_error_exp(error_offset, "number");
}

template <typename Istream>
inline bool raw_ascii_reader<Istream>::read_bool(void)
{
    std::size_t error_offset;
    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

    if (m_stream.peek() == 't')
    {
        m_stream.take();
        if (m_stream.end() || m_stream.take() != 'r') goto fail;
        if (m_stream.end() || m_stream.take() != 'u') goto fail;
        if (m_stream.end() || m_stream.take() != 'e') goto fail;
        return true;
    }
    else if (m_stream.peek() == 'f')
    {
        m_stream.take();
        if (m_stream.end() || m_stream.take() != 'a') goto fail;
        if (m_stream.end() || m_stream.take() != 'l') goto fail;
        if (m_stream.end() || m_stream.take() != 's') goto fail;
        if (m_stream.end() || m_stream.take() != 'e') goto fail;
        return false;
    }
fail:
    throw iutil::parse_error_exp(error_offset, "bool");
}

template <typename Istream>
inline void raw_ascii_reader<Istream>::read_null(void)
{
    std::size_t error_offset;
    if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

    if (m_stream.end() || m_stream.take() != 'n') goto fail;
    if (m_stream.end() || m_stream.take() != 'u') goto fail;
    if (m_stream.end() || m_stream.take() != 'l') goto fail;
    if (m_stream.end() || m_stream.take() != 'l') goto fail;
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

template <typename Istream>
template <typename Ostream>
inline void raw_ascii_reader<Istream>::take_unescape(JsonIstream& is, Ostream& os)
{
    assert(!is.end());
    const char* EXSTR_bad_escape = "Invalid escape sequence.";

    if (is.peek() != '\\')
        os.put(is.take());
    else
    {
        is.take(); // take '\'

        if (is.end())
            throw iutil::parse_error(is.inpos(), EXSTR_bad_escape);

        switch (is.take()) // unescape
        {
            case 'b': os.put('\b'); break;
            case 'f': os.put('\f'); break;
            case 'n': os.put('\n'); break;
            case 'r': os.put('\r'); break;
            case 't': os.put('\t'); break;
            case '"': os.put('"'); break;
            case '/': os.put('/'); break; // MS-only?
            default: 
                throw iutil::parse_error(is.inpos() - 1, EXSTR_bad_escape);
        }
    }
}

template <typename Istream>
template <typename Ostream>
inline void raw_ascii_reader<Istream>::read_string_impl(JsonIstream& is, Ostream& os)
{
    if (!iutil::skip_ws(is) || is.peek() != '"')
        goto fail;

    is.take(); // open quotes
    while (!is.end() && is.peek() != '"')
        take_unescape(is, os);

    if (is.end()) goto fail;
    is.take(); // close quotes
    return;

fail:
    throw iutil::parse_error_exp(is.inpos(), "string");
}

template <typename Istream>
template <typename Func>
inline bool
raw_ascii_reader<Istream>::read_string_or_null_impl(JsonIstream& is, Func get_os)
{
    if (!iutil::skip_ws(is)) goto fail;

    if (is.peek() == 'n')
    {
        is.take();
        if (is.end() || is.take() != 'u') goto fail;
        if (is.end() || is.take() != 'l') goto fail;
        if (is.end() || is.take() != 'l') goto fail;
        return false;
    }
    else if (is.peek() == '"')
    {
        auto&& os = get_os();

        is.take(); // open quotes
        while (!is.end() && is.peek() != '"')
            take_unescape(is, os);

        if (is.end()) goto fail;
        is.take(); // close quotes

        return true;
    }
fail:
    throw iutil::parse_error_exp(is.inpos(), "string or null");
}

template <typename Istream>
template <typename Ostream>
inline void raw_ascii_reader<Istream>::read_string(Ostream& os, bool quoted)
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

template <typename Istream, typename AllocatorPolicy>
inline void ascii_reader<Istream, AllocatorPolicy>::read_separator(void)
{
    if (this->m_nodes.top().has_children)
    {
        switch (this->m_nodes.top().type)
        {
            case DOCNODE_object:
            case DOCNODE_array: m_rr.read_item_separator(); break;
            case DOCNODE_root: throw std::runtime_error(internal::EXSTR_multi_root);
            default: assert(false);
        }
    }
    // key node is popped before it can have children
    else if (this->m_nodes.top().type == DOCNODE_key)
        m_rr.read_key_separator();
}

template <typename Istream, typename AllocatorPolicy>
template <typename Traits, typename Allocator>
inline std::basic_string<char, Traits, Allocator> ascii_reader<Istream, AllocatorPolicy>::read_key(void)
{
    this->template assert_rule<DOCNODE_key>();

    read_separator();
    auto str = m_rr.template read_string<Traits, Allocator>();

    this->m_nodes.push({ DOCNODE_key });
    // don't end_child_node(), key-value pair is incomplete
    return str;
}

template <typename Istream, typename AllocatorPolicy>
template <typename Traits, typename IsEndpFunc>
inline bool ascii_reader<Istream, AllocatorPolicy>::read_key_impl(const char* str, IsEndpFunc is_endp, std::size_t& out_pos)
{
    this->template assert_rule<DOCNODE_key>();

    read_separator();
    iutil::skip_ws(m_rr.stream(), out_pos);

    internal::streq_ostream<Traits, IsEndpFunc> os(str, is_endp);
    m_rr.read_string(os);

    this->m_nodes.push({ DOCNODE_key });
    // don't end_child_node(), key-value pair is incomplete

    return os.str_is_equal();
}

template <typename Istream, typename AllocatorPolicy>
template <typename Traits, typename Allocator>
inline void ascii_reader<Istream, AllocatorPolicy>::read_key(const std::basic_string<char, Traits, Allocator>& expected_key)
{
    auto is_endp = [&](const char* p) { return p == expected_key.data() + expected_key.size(); };

    std::size_t startpos;
    if (!read_key_impl<Traits>(expected_key.data(), is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key.data(), expected_key.length()) + "\"");
}

template <typename Istream, typename AllocatorPolicy>
inline void ascii_reader<Istream, AllocatorPolicy>::read_key(const char* expected_key)
{
    auto is_endp = [](const char* p) { return *p == '\0'; };

    std::size_t startpos;
    if (!read_key_impl<std::char_traits<char>>(expected_key, is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key) + "\"");
}

template <typename Istream, typename AllocatorPolicy>
inline void ascii_reader<Istream, AllocatorPolicy>::read_key(const char* expected_key, std::size_t length)
{
    auto is_endp = [&](const char* p) { return p == expected_key + length; };

    std::size_t startpos;
    if (!read_key_impl<std::char_traits<char>>(expected_key, is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key, length) + "\"");
}

template <typename Istream, typename AllocatorPolicy>
template <typename Value>
inline Value ascii_reader<Istream, AllocatorPolicy>::read_value(void)
{
    this->template assert_rule<DOCNODE_value>();

    read_separator();
    Value value = m_rr.template read<Value>();

    this->end_child_node();
    return value;
}

template <typename Istream, typename AllocatorPolicy>
template <typename Key, typename Value>
inline std::pair<Key, Value> ascii_reader<Istream, AllocatorPolicy>::read_key_value(void)
{
    static_assert(iutil::is_instance_of_basic_string<Key, char>::value, 
        "Key must be a std::basic_string with value_type char.");

    using KeyTraits = typename Key::traits_type;
    using KeyAllocator = typename Key::allocator_type;

    this->template assert_rule<DOCNODE_key, DOCNODE_value>();

    if (this->m_nodes.top().has_children)
        m_rr.read_item_separator();

    Key key = m_rr.template read_string<KeyTraits, KeyAllocator>();
    m_rr.read_key_separator();
    Value value = m_rr.template read<Value>();

    this->end_child_node();
    return { std::move(key), std::move(value) };
}

}

#endif
