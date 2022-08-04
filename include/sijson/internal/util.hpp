//
// Internal utilities.
// Should not have dependencies outside of internal/.
//

#ifndef SIJSON_INTERNAL_UTIL_HPP
#define SIJSON_INTERNAL_UTIL_HPP

#include <cassert>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <type_traits>
#include <string>
#include <iostream>
#include <utility>
#include <algorithm>
#include <stdexcept>

#include "config.hpp"

#if SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif


namespace sijson {
namespace internal {
namespace util {

template <bool test, typename T = int>
using enable_if_t = typename std::enable_if<test, T>::type;

template <typename A, typename B, typename T = int>
using enable_if_same_t = enable_if_t<std::is_same<A, B>::value, T>;


template <bool B, typename T, T ValIfTrue, T ValIfFalse>
struct conditional_constant : std::integral_constant<T, ValIfTrue> {};

template <typename T, T ValIfTrue, T ValIfFalse>
struct conditional_constant<false, T, ValIfTrue, ValIfFalse> 
    : std::integral_constant<T, ValIfFalse> 
{};


// True if T is an instance of the template Templ.
template <typename T, template <typename...> class Templ>
struct is_instance_of : std::false_type {};

template <typename ...Templ_args, template <typename...> class Templ>
struct is_instance_of<Templ<Templ_args...>, Templ> : std::true_type {};


#if defined (__cpp_lib_logical_traits) || SIJSON_CPLUSPLUS >= 201703L
template <typename ...Bn>
using conjunction = std::conjunction<Bn...>;

template <typename ...Bn>
using disjunction = std::disjunction<Bn...>;

template <typename B>
using negation = std::negation<B>;
#else
// https://en.cppreference.com/w/cpp/types/conjunction, modified for C++11
template <typename...> struct conjunction : std::true_type {};
template <typename B1> struct conjunction<B1> : B1 { };
template <typename B1, typename ...Bn>
struct conjunction<B1, Bn...> : std::conditional<bool(B1::value), conjunction<Bn...>, B1>::type {};

// https://en.cppreference.com/w/cpp/types/disjunction, modified for C++11
template <typename...> struct disjunction : std::false_type {};
template <typename B1> struct disjunction<B1> : B1 { };
template <typename B1, typename ...Bn>
struct disjunction<B1, Bn...> : std::conditional<bool(B1::value), B1, disjunction<Bn...>>::type {};

// https://en.cppreference.com/w/cpp/types/negation, modified for C++11
template <typename B>
struct negation : std::integral_constant<bool, !bool(B::value)> {};
#endif


#if defined(__cpp_lib_void_t) || SIJSON_CPLUSPLUS >= 201703L
template <typename ...Ts>
using void_t = std::void_t<Ts...>;
#else
// https://en.cppreference.com/w/cpp/types/void_t
// https://cplusplus.github.io/CWG/issues/1558.html
template <typename ...Ts> struct make_void { using type = void; };
template <typename ...Ts> using void_t = typename make_void<Ts...>::type;
#endif


#ifdef __cpp_lib_remove_cvref
template <typename T>
using remove_cvref_t = std::remove_cvref_t<T>;
#else
template <typename T>
using remove_cvref_t = typename std::remove_cv<
    typename std::remove_reference<T>::type>::type;
#endif


static constexpr auto int32_max = 0x7FFFFFFF;
static constexpr auto int32_min = -0x7FFFFFFF - 1;
static constexpr auto uint32_max = 0xFFFFFFFF;

static constexpr auto int64_max = 0x7FFFFFFFFFFFFFFF;
static constexpr auto int64_min = -0x7FFFFFFFFFFFFFFF - 1;
static constexpr auto uint64_max = 0xFFFFFFFFFFFFFFFF;

template <typename T> struct least_t_exp_min {};
template <> struct least_t_exp_min<std::int_least32_t> : std::integral_constant<std::int_least32_t, int32_min> {};
template <> struct least_t_exp_min<std::int_least64_t> : std::integral_constant<std::int_least64_t, int64_min> {};
template <> struct least_t_exp_min<std::uint_least32_t> : std::integral_constant<std::uint_least32_t, 0> {};
template <> struct least_t_exp_min<std::uint_least64_t> : std::integral_constant<std::uint_least64_t, 0> {};

template <typename T> struct least_t_exp_max {};
template <> struct least_t_exp_max<std::int_least32_t> : std::integral_constant<std::int_least32_t, int32_max> {};
template <> struct least_t_exp_max<std::int_least64_t> : std::integral_constant<std::int_least64_t, int64_max> {};
template <> struct least_t_exp_max<std::uint_least32_t> : std::integral_constant<std::uint_least32_t, uint32_max> {};
template <> struct least_t_exp_max<std::uint_least64_t> : std::integral_constant<std::uint_least64_t, uint64_max> {};


// Arrays decay to const T*, all other Ts stay the same.
// See https://stackoverflow.com/questions/12374746/.
template <typename T>
using decay_array_to_constptr_t = typename std::decay<const T&>::type;


template <typename T>
using is_nonbool_integral = std::integral_constant<bool,
    std::is_integral<T>::value && !std::is_same<T, bool>::value>;

template <typename T>
using is_nb_signed_integral = std::integral_constant<bool,
    is_nonbool_integral<T>::value && std::is_signed<T>::value>;

template <typename T>
using is_nb_unsigned_integral = std::integral_constant<bool,
    is_nonbool_integral<T>::value && std::is_unsigned<T>::value>;


// True if T is a POD type (trivial and standard-layout).
template <typename T>
using is_pod = std::integral_constant<bool,
    std::is_standard_layout<T>::value &&
    std::is_trivial<T>::value &&
    std::is_trivially_destructible<T>::value>;


template <typename T, typename CharT>
struct has_value_type : std::integral_constant<bool, 
    std::is_same<typename T::value_type, CharT>::value> 
{};

// True if T is basic_string<CharT, ...>.
template <typename T, typename CharT>
using is_instance_of_basic_string = std::integral_constant<bool, conjunction<
    is_instance_of<T, std::basic_string>, has_value_type<T, CharT>>::value>;

#if SIJSON_HAS_STRING_VIEW
// True if T is basic_string_view<CharT, ...>.
template <typename T, typename CharT>
using is_instance_of_basic_string_view = std::integral_constant<bool, conjunction<
    is_instance_of<T, std::basic_string_view>, has_value_type<T, CharT>>::value>;
#endif

// True if T is a string type supported by this library for writing.
template <typename T, typename CharT>
using is_writable_string_type = disjunction<
    std::is_same<typename std::decay<T>::type, CharT*>,
    std::is_same<typename std::decay<T>::type, const CharT*>,
    is_instance_of_basic_string<T, CharT>
#if SIJSON_HAS_STRING_VIEW
    ,is_instance_of_basic_string_view<T, CharT>
#endif
>;

// True if T is a string type supported by this library for reading.
template <typename T, typename CharT>
using is_readable_string_type = is_instance_of_basic_string<T, CharT>;


template <typename T, typename = void>
struct inherits_std_basic_istream : std::false_type {};

template <typename T>
struct inherits_std_basic_istream<T,
    void_t<std::is_convertible<T*,
        std::basic_istream<typename T::char_type, typename T::traits_type>*>>> :
    std::is_convertible<T*, std::basic_istream<typename T::char_type, typename T::traits_type>*>
{};

template <typename T, typename = void>
struct inherits_std_basic_ostream : std::false_type {};

template <typename T>
struct inherits_std_basic_ostream<T,
    void_t<std::is_convertible<T*,
        std::basic_ostream<typename T::char_type, typename T::traits_type>*>>> :
    std::is_convertible<T*, std::basic_ostream<typename T::char_type, typename T::traits_type>*>
{};


// Max # of chars required to represent a numerical value as decimal text (includes sign).
template <typename T>
using max_chars10 = conditional_constant<
    std::is_floating_point<T>::value, int,
    // max precision + exponent + sign + point + e
    std::numeric_limits<T>::max_digits10 + (std::numeric_limits<int>::digits10 + 2) + 3,
    // round up digits10 + sign
    std::numeric_limits<T>::digits10 + 1 + std::is_signed<T>::value>;


template <typename T>
inline void move_if_tag(T& lhs, T&& rhs, std::true_type)
{ lhs = std::move(rhs); }

template <typename T>
inline void move_if_tag(T&, T&&, std::false_type) {}

template <typename T>
inline void copy_if_tag(T& lhs, const T& rhs, std::true_type)
{ lhs = rhs; }

template <typename T>
inline void copy_if_tag(T&, const T&, std::false_type) {}


// Absolute value of a signed integer. Converts to unsigned.
template <typename T,
    typename uT = typename std::make_unsigned<T>::type>
constexpr uT absu(T value) noexcept
{
    // should be true on sane archs :)
    static_assert(
        std::numeric_limits<T>::max() + std::numeric_limits<T>::min() == 0 ||
        std::numeric_limits<uT>::digits > std::numeric_limits<T>::digits,
        "absu() may fail for values close to signed min()");

    return value < 0 ? -static_cast<uT>(value) : value;
}

// Negate an unsigned value. Converts to signed.
// Behavior is undefined if the value is not convertible.
template <typename T,
    T lbound = std::numeric_limits<T>::min(),
    T ubound = std::numeric_limits<T>::max(),
    typename uT = typename std::make_unsigned<T>::type>
inline
#ifdef NDEBUG
constexpr
#endif
T uneg(uT uvalue) noexcept
{
    static_assert(lbound < 0 && ubound > 0 &&
        absu(lbound) >= ubound, "Invalid lower or upper bound");

#ifndef NDEBUG
    assert(uvalue <= absu(lbound));
#endif
    return uvalue <= ubound ? -static_cast<T>(uvalue) :
        -ubound - static_cast<T>(uvalue - ubound);
}

// Round an unsigned value up to the closest multiple of factor.
template <typename T>
constexpr T uround_up(T value, T factor)
{
    static_assert(std::is_unsigned<T>::value, "T must be unsigned");   
    return value != 0 && factor != 0 ? (value + factor - 1) - (value + factor - 1) % factor : 0;
}


inline bool is_digit(char c) noexcept { return c >= '0' && c <= '9'; }

inline bool is_ws(char c) noexcept
{
    switch (c)
    {
        case 0x20: // space
        case 0x09: // horizontal tab
        case 0x0a: // line feed
        case 0x0d: // carriage return
            return true;
        default: 
            return false;
    }
}

inline char to_lower(char c) noexcept { return c >= 'A' && c <= 'Z' ? c - ('A' - 'a') : c; }

template <typename const_iterator, typename pred>
inline bool starts_with(const_iterator str_begin,
    const_iterator str_end, const std::string& needle, pred is_equal)
{
    assert(str_end >= str_begin);
    auto str = str_begin;
    for (std::size_t i = 0; i < needle.length(); ++i)
    {
        if (str == str_end) return false;
        if (!is_equal(*str, needle[i])) return false;
        str++;
    }
    return true;
}

// Returns true if stream has more characters.
template <typename Istream>
inline bool skip_ws(Istream& stream)
{
    while (!stream.end() && is_ws(stream.peek())) {
        stream.take();
    }
    return !stream.end();
}

// Returns true if stream has more characters.
template <typename Istream>
inline bool skip_ws(Istream& stream, std::size_t& out_finalpos)
{
    bool rval = skip_ws(stream);
    out_finalpos = stream.inpos();
    return rval;
}

inline std::runtime_error parse_error(std::size_t pos, const std::string& message)
{
    return std::runtime_error("Parse error at offset " + std::to_string(pos) + ": " + message);
}
inline std::runtime_error parse_error_exp(std::size_t pos, const std::string& expected)
{
    return parse_error(pos, "expected " + expected);
}
}}

// Utility functions/definitions (internal use only).
namespace iutil = internal::util;
}

#endif