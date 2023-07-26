
#ifndef SIJSON_UTILITY_HPP
#define SIJSON_UTILITY_HPP

#include <cstddef>
#include <type_traits>
#include <memory>
#include <string>
#include <utility>

#include "internal/core.hpp"

#include "io_string.hpp"
#include "io_wrappers.hpp"
#include "writer.hpp"
#include "reader.hpp"

#ifdef SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif


namespace sijson {

// Convert value to string.
template <
    typename CharT = char,
    typename Allocator = std::allocator<CharT>,
    typename Value>
inline iutil::stdstr<CharT, Allocator> to_string(const Value& value)
{
    static_assert(
        !iutil::is_writable_string_type<Value, CharT>::value,
        "Input is already a string.");

    basic_out_stdstr<CharT, Allocator> os;
    raw_ascii_writer<decltype(os)> w(os);
    w.write(value);

    return std::move(os).str();
}

// -------------------------------------------------------------------

// Convert string to value.
template <typename Value, typename CharT>
inline Value to_value(const CharT* str, std::size_t length)
{
    static_assert(
        !iutil::is_readable_string_type<Value, CharT>::value,
        "Input is already a string.");

    in_str is(str, length);
    raw_reader<in_str, no_whitespace> r(is);
    return r.template read<Value>();
}

// Convert string to value.
template <typename Value, typename CharT>
inline Value to_value(const CharT* str)
{
    return to_value<Value>(str, std::char_traits<CharT>::length(str));
}

#ifdef SIJSON_HAS_STRING_VIEW
// Convert string to value.
template <typename Value, typename CharT>
inline Value to_value(std::basic_string_view<CharT> str)
{
    return to_value<Value>(str.data(), str.length());
}
#endif

// Convert string to value.
template <typename Value, typename CharT, typename Allocator>
inline Value to_value(const iutil::stdstr<CharT, Allocator>& str)
{
    return to_value<Value>(str.data(), str.length());
}

// -------------------------------------------------------------------

// Escape string.
template <typename CharT, typename Allocator = std::allocator<CharT>>
inline iutil::stdstr<CharT, Allocator> escape(const CharT* str, std::size_t length)
{
    in_str is(str, length);
    basic_out_stdstr<CharT, Allocator> os;

    raw_ascii_writer<decltype(os)> w(os);
    w.write_string_from(is, false);

    return std::move(os).str();
}

// Escape string.
template <typename CharT, typename Allocator = std::allocator<CharT>>
inline iutil::stdstr<CharT, Allocator> escape(const CharT* str)
{
    return escape<CharT, Allocator>(str, std::char_traits<CharT>::length(str));
}

#ifdef SIJSON_HAS_STRING_VIEW
// Escape string.
template <typename CharT, typename Allocator = std::allocator<CharT>>
inline iutil::stdstr<CharT, Allocator> escape(std::basic_string_view<CharT> str)
{
    return escape<CharT, Allocator>(str.data(), str.length());
}
#endif

// Escape string.
template <typename CharT, typename Allocator>
inline iutil::stdstr<CharT, Allocator> escape(const iutil::stdstr<CharT, Allocator>& str)
{
    return escape<CharT, Allocator>(str.data(), str.length());
}

// -------------------------------------------------------------------

// Unescape string.
template <typename CharT, typename Allocator = std::allocator<CharT>>
inline iutil::stdstr<CharT, Allocator> unescape(const CharT* str, std::size_t length)
{
    in_str is(str, length);
    basic_out_stdstr<CharT, Allocator> os;

    raw_reader<in_str> r(is);
    r.read_string_to(os, RDFLAG_str_only);

    return std::move(os).str();
}

// Unescape string.
template <typename CharT, typename Allocator = std::allocator<CharT>>
inline iutil::stdstr<CharT, Allocator> unescape(const CharT* str)
{
    return unescape<CharT, Allocator>(str, std::char_traits<CharT>::length(str));
}

#ifdef SIJSON_HAS_STRING_VIEW
// Unescape string.
template <typename CharT, typename Allocator = std::allocator<CharT>>
inline iutil::stdstr<CharT, Allocator> unescape(std::basic_string_view<CharT> str)
{
    return unescape<CharT, Allocator>(str.data(), str.length());
}
#endif

// Unescape string.
template <typename CharT, typename Allocator>
inline iutil::stdstr<CharT, Allocator> unescape(const iutil::stdstr<CharT, Allocator>& str)
{
    return unescape<CharT, Allocator>(str.data(), str.length());
}

// -------------------------------------------------------------------

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
            case TOKEN_start_object:
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
                    w.write_ws(m_tab_size * (depth + 1));

                    r.read_string_to(w.stream(), RDFLAG_str_copy);
                    r.read_key_separator();
                    w.write_key_separator();
                    w.stream().put(' ');

                    print(depth + 1);
                    item_sep = true;
                }

                r.read_end_object();
                w.write_newline();
                w.write_ws(m_tab_size * depth);
                w.write_end_object();
            }
            break;

            case TOKEN_start_array:
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
                    w.write_ws(m_tab_size * (depth + 1));

                    print(depth + 1);
                    item_sep = true;
                }

                r.read_end_array();
                w.write_newline();
                w.write_ws(m_tab_size * depth);
                w.write_end_array();
            }
            break;

            case TOKEN_number:
                w.write_number(r.read_number());
                break;

            case TOKEN_string:
                r.read_string_to(w.stream(), RDFLAG_str_copy);
                break;

            case TOKEN_boolean:
                w.write_bool(r.read_bool());
                break;

            case TOKEN_null:
                r.read_null();
                w.write_null();
                break;

            case TOKEN_eof: 
                break;

            case TOKEN_invalid:
                throw parse_error(r.in().ipos(), "invalid token");

            default:
#ifndef __cpp_lib_unreachable
                SIJSON_ASSERT(false);
                break;    
#else                 
                std::unreachable();
#endif
        }
    }

private:
    raw_reader<Istream> r;
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
    out_stdstr os;
    pretty_print(stream, os, tab_size);
    return std::move(os).str();
}

inline std::string pretty_print(const char* json, std::size_t len, unsigned tab_size = 2)
{
    in_str is(json, len);
    return pretty_print(is, tab_size);
}

inline std::string pretty_print(const char* json, unsigned tab_size = 2)
{
    in_str is(json);
    return pretty_print(is, tab_size);
}

inline std::string pretty_print(const std::string& json, unsigned tab_size = 2)
{
    return pretty_print(json.c_str(), json.length(), tab_size);
}

}

#endif