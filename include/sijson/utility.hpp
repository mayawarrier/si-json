
#ifndef SIJSON_UTILITY_HPP
#define SIJSON_UTILITY_HPP

#include <cstddef>
#include <type_traits>
#include <memory>
#include <string>
#include <utility>

#include "internal/config.hpp"
#include "internal/util.hpp"

#include "stringstream.hpp"
#include "stream_adapters.hpp"
#include "writer.hpp"
#include "reader.hpp"

#if SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif


namespace sijson {

namespace internal 
{
template <typename Value, typename Istream>
inline Value to_value_impl(Istream& is)
{
    static_assert(!iutil::is_readable_string_type<Value, char>::value,
        "Input is already a string.");

    raw_ascii_reader<Istream> reader(is);
    return reader.template read<Value>();
}

template <typename DestString, typename Istream>
inline DestString escape_impl(Istream& is)
{
    static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
        "DestString is not a std::basic_string.");

    using Allocator = typename DestString::allocator_type;

    basic_ostdstrstream<char, Allocator> os;
    raw_ascii_writer<decltype(os)> writer(os);
    writer.write_string_from(is, false);

    return std::move(os).str();
}

template <typename DestString, typename Istream>
inline DestString unescape_impl(Istream& is)
{
    static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
        "DestString is not a std::basic_string.");

    using Allocator = typename DestString::allocator_type;

    basic_ostdstrstream<char, Allocator> os;
    raw_ascii_reader<Istream> reader(is);
    reader.read_string_into(os, false);

    return std::move(os).str();
}
}

// Convert value to string.
template <
    typename Allocator = std::allocator<char>,
    typename Value>
    inline std::basic_string<char, std::char_traits<char>, Allocator> to_string(const Value& value)
{
    static_assert(!iutil::is_writable_string_type<Value, char>::value,
        "Input is already a string.");

    basic_ostdstrstream<char, Allocator> os;
    raw_ascii_writer<decltype(os)> writer(os);
    writer.write(value);

    return std::move(os).str();
}

// Convert value to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString, typename Value>
inline DestString to_string_as(const Value& value)
{
    static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
        "DestString is not a std::basic_string.");

    return to_string<typename DestString::allocator_type>(value);
}

// -------------------------------------------------------------------

// Convert string to value.
template <typename Value>
inline Value to_value(const char* string)
{
    istrstream is(string);
    return internal::to_value_impl<Value>(is);
}

// Convert string to value.
template <typename Value>
inline Value to_value(const char* string, std::size_t length)
{
    istrstream is(string, length);
    return internal::to_value_impl<Value>(is);
}

#if SIJSON_HAS_STRING_VIEW
// Convert string to value.
template <typename Value>
inline Value to_value(std::basic_string_view<char, std::char_traits<char>> strview)
{
    istrstream is(strview);
    return internal::to_value_impl<Value>(is);
}
#endif

// Convert string to value.
template <typename Value, typename ...Ts>
inline Value to_value(const std::basic_string<char, Ts...>& string)
{
    istrstream is(string);
    return internal::to_value_impl<Value>(is);
}

// -------------------------------------------------------------------

// Escape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString> 
inline DestString escape_as(const char* string)
{
    istrstream is(string);
    return internal::escape_impl<DestString>(is);
}

// Escape string.
inline std::string escape(const char* string)
{
    return escape_as<std::string>(string);
}

// Escape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString>
inline DestString escape_as(const char* string, std::size_t length)
{
    istrstream is(string, length);
    return internal::escape_impl<DestString>(is);
}

// Escape string.
inline std::string escape(const char* string, std::size_t length)
{
    return escape_as<std::string>(string, length);
}

#if SIJSON_HAS_STRING_VIEW
// Escape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString, typename SrcStringView,
    iutil::enable_if_t<iutil::is_instance_of_basic_string_view<SrcStringView, char>::value> = 0>
    inline DestString escape_as(SrcStringView strview)
{
    istrstream is(strview);
    return internal::escape_impl<DestString>(is);
}

// Escape string.
inline std::string escape(std::string_view strview)
{
    return escape_as<std::string>(strview);
}
#endif

// Escape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString, typename SrcString,
    iutil::enable_if_t<iutil::is_instance_of_basic_string<SrcString, char>::value> = 0>
inline DestString escape_as(const SrcString& string)
{
    istrstream is(string);
    return internal::escape_impl<DestString>(is);
}

// Escape string.
template <typename Allocator>
inline std::basic_string<std::char_traits<char>, Allocator> escape(const std::basic_string<std::char_traits<char>, Allocator>& string)
{
    istrstream is(string);
    return internal::escape_impl<std::basic_string<std::char_traits<char>, Allocator>>(is);
}

// -------------------------------------------------------------------

// Unescape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString>
inline DestString unescape_as(const char* string)
{
    istrstream is(string);
    return internal::unescape_impl<DestString>(is);
}

// Unescape string.
inline std::string unescape(const char* string)
{
    return unescape_as<std::string>(string);
}

// Unescape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString>
inline DestString unescape_as(const char* string, std::size_t length)
{
    istrstream is(string, length);
    return internal::unescape_impl<DestString>(is);
}

// Unescape string.
inline std::string unescape(const char* string, std::size_t length)
{
    return unescape_as<std::string>(string, length);
}

#if SIJSON_HAS_STRING_VIEW
// Unescape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString, typename SrcStringView,
    iutil::enable_if_t<iutil::is_instance_of_basic_string_view<SrcStringView, char>::value> = 0>
    inline DestString unescape_as(SrcStringView strview)
{
    istrstream is(strview);
    return internal::unescape_impl<DestString>(is);
}

// Unescape string.
inline std::string unescape(std::string_view strview)
{
    return unescape_as<std::string>(strview);
}
#endif

// Unescape to string of custom type.
// DestString must be a std::basic_string.
template <typename DestString, typename SrcString,
    iutil::enable_if_t<iutil::is_instance_of_basic_string<SrcString, char>::value> = 0>
    inline DestString unescape_as(const SrcString& string)
{
    istrstream is(string);
    return internal::unescape_impl<DestString>(is);
}

// Unescape string.
template <typename Allocator>
inline std::basic_string<std::char_traits<char>, Allocator> unescape(const std::basic_string<std::char_traits<char>, Allocator>& string)
{
    return unescape_as<std::basic_string<std::char_traits<char>, Allocator>>(string);
}

// -------------------------------------------------------------------

// Transfer quoted string from src to dest without modification.
template <typename Istream, typename Ostream>
    inline void transfer_string(Istream& src, Ostream& dest)
{
    sijson_istream_t<Istream&> is(src);
    sijson_ostream_t<Ostream&> os(dest);

    if (!iutil::skip_ws(is) || is.peek() != 0x22) // '"'
        goto fail;

    is.take(); // open quotes
    os.put(0x22); // '"'
    while (!is.end() && is.peek() != 0x22) // '"'
    {
        if (is.peek() != 0x5c) // '\'
            os.put(is.take());
        else
        {
            os.put(is.take());
            if (is.end()) goto fail;
            os.put(is.take());
        }
    }
    
    if (is.end()) goto fail;
    is.take(); // close quotes
    os.put(0x22); // '"'

    return;
fail:
    throw iutil::parse_error_exp(is.inpos(), "string");
}


template <typename Istream, typename Ostream>
class pretty_printer
{
public:
    pretty_printer(Istream& is, Ostream& os, unsigned tab_size = 2) :
        r{ is }, w{ os }, m_tab_size(tab_size)
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
                    w.write_whitespace(m_tab_size * (depth + 1));

                    transfer_string(r.stream(), w.stream());
                    r.read_key_separator();
                    w.write_key_separator();
                    w.stream().put(' ');

                    print(depth + 1);
                    item_sep = true;
                }

                r.read_end_object();
                w.write_newline();
                w.write_whitespace(m_tab_size * depth);
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
                    w.write_whitespace(m_tab_size * (depth + 1));

                    print(depth + 1);
                    item_sep = true;
                }

                r.read_end_array();
                w.write_newline();
                w.write_whitespace(m_tab_size * depth);
                w.write_end_array();
            }
            break;

            case TOKEN_number:
                w.write_number(r.read_number());
                break;

            case TOKEN_string:
                transfer_string(r.stream(), w.stream());
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

private:
    raw_ascii_reader<Istream> r;
    raw_ascii_writer<Ostream> w;
    unsigned m_tab_size;
};


template <typename Istream, typename Ostream>
inline void pretty_print(Istream& in, Ostream& out, unsigned tab_size = 2)
{
    pretty_printer<Istream, Ostream> pp(in, out, tab_size);
    pp.print();
}

template <typename Istream>
inline std::string pretty_print(Istream& stream, unsigned tab_size = 2)
{
    ostdstrstream os(4);
    pretty_print(stream, os, tab_size);
    return std::move(os).str();
}

inline std::string pretty_print(const char* json, std::size_t len, unsigned tab_size = 2)
{
    istrstream is(json, len);
    return pretty_print(is, tab_size);
}

inline std::string pretty_print(const char* json, unsigned tab_size = 2)
{
    istrstream is(json);
    return pretty_print(is, tab_size);
}

inline std::string pretty_print(const std::string& json, unsigned tab_size = 2)
{
    return pretty_print(json.c_str(), json.length(), tab_size);
}

}

#endif