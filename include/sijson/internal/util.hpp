//
// Internal utilities.
// Should not have dependencies outside of internal/.
//

#ifndef SIJSON_INTERNAL_UTIL_HPP
#define SIJSON_INTERNAL_UTIL_HPP

#include <cstddef>
#include <cassert>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <tuple>
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

// abi::__cxa_demangle()
#if SIJSON_HAS_CXXABI_H
#include <cxxabi.h>
#endif


namespace sijson {
namespace internal {
namespace util {

template <bool test, typename T = int>
using enable_if_t = typename std::enable_if<test, T>::type;

template <typename A, typename B, typename T = int>
using enable_if_same_t = enable_if_t<std::is_same<A, B>::value, T>;


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

template <typename T>
using remove_reference_t = typename std::remove_reference<T>::type;


// https://en.cppreference.com/w/cpp/utility/in_place
struct in_place_t
{
    constexpr explicit in_place_t() = default;
};
static constexpr in_place_t in_place{};


// Workaround for std::allocator<void> deprecation warnings 
// even though it only exists as a placeholder for rebind
struct placeholder {};


// True if T is an instance of the template Templ.
template <typename T, template <typename...> class Templ>
struct is_instance_of : std::false_type {};

template <typename ...TemplArgs, template <typename...> class Templ>
struct is_instance_of<Templ<TemplArgs...>, Templ> : std::true_type {};


template <typename T, bool Enable>
struct add_const_if { using type = T; };

template <typename T>
struct add_const_if<T, true> { using type = const T; };

template <typename T, bool Enable>
using add_const_if_t = typename add_const_if<T, Enable>::type;


template <typename Tuple, typename T>
struct tuple_has_type;

template <typename T, typename ...Us>
struct tuple_has_type<std::tuple<Us...>, T> : disjunction<std::is_same<T, Us>...>
{};


static constexpr auto int32_max = 0x7FFFFFFF;
static constexpr auto int32_min = -0x7FFFFFFF - 1;
static constexpr auto uint32_max = 0xFFFFFFFF;

static constexpr auto int64_max = 0x7FFFFFFFFFFFFFFF;
static constexpr auto int64_min = -0x7FFFFFFFFFFFFFFF - 1;
static constexpr auto uint64_max = 0xFFFFFFFFFFFFFFFF;


template <typename T>
inline std::string nameof(void)
{
#ifdef _MSC_VER
// MSVC demangles these already
return typeid(T).name();

#elif SIJSON_HAS_CXXABI_H
    int status = -1;
    std::size_t size = 0;
    char* name = abi::__cxa_demangle(typeid(T).name(), NULL, &size, &status);

    std::string sname;
    if (name)
    {
        sname.assign(name, size);
        std::free(name);
    }
    else {
        sname.assign("typeid: ");
        sname.append(typeid(T).name());
    }
    return sname;
#else
    return std::string("typeid: ") + typeid(T).name();
#endif
}


// Arrays decay to const T*, all other Ts stay the same.
// See https://stackoverflow.com/questions/12374746/.
template <typename T>
using decay_array_to_constptr_t = typename std::decay<const T&>::type;


template <typename T>
using make_unsigned_t = typename std::make_unsigned<T>::type;

template <typename T>
using make_signed_t = typename std::make_signed<T>::type;

template <typename T>
using is_nb_signed_integral = std::integral_constant<bool,
    !std::is_same<T, bool>::value && std::is_integral<T>::value && std::is_signed<T>::value>;

template <typename T>
using is_nb_unsigned_integral = std::integral_constant<bool,
    !std::is_same<T, bool>::value && std::is_integral<T>::value && std::is_unsigned<T>::value>;

// True if T is a POD type (trivial and standard-layout).
template <typename T>
using is_pod = std::integral_constant<bool,
    std::is_standard_layout<T>::value &&
    std::is_trivial<T>::value &&
    std::is_trivially_destructible<T>::value>;


template <typename T, typename ValueT>
struct has_value_type : std::integral_constant<bool, 
    std::is_same<typename T::value_type, ValueT>::value> 
{};

template <typename T, typename Traits>
struct has_traits_type : std::integral_constant<bool,
    std::is_same<typename T::traits_type, Traits>::value>
{};

// True if T is std::basic_string<CharT, Traits, ...>.
template <typename T, typename CharT, typename Traits = std::char_traits<CharT>>
using is_instance_of_basic_string = conjunction<
    is_instance_of<T, std::basic_string>, has_value_type<T, CharT>, has_traits_type<T, Traits>>;

#if SIJSON_HAS_STRING_VIEW
// True if T is basic_string_view<CharT, ...>.
template <typename T, typename CharT>
using is_instance_of_basic_string_view = conjunction<
    is_instance_of<T, std::basic_string_view>, has_value_type<T, CharT>>;
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
template <typename T, bool = std::is_floating_point<T>::value>
struct max_chars10 : std::integral_constant<int,
    // max precision + exponent + sign + point + e
    std::numeric_limits<T>::max_digits10 + (std::numeric_limits<int>::digits10 + 2) + 3>
{};

// Max # of chars required to represent a numerical value as decimal text (includes sign).
template <typename T>
struct max_chars10<T, false> : std::integral_constant<int,
    // round up digits10 + sign
    std::numeric_limits<T>::digits10 + 1 + std::is_signed<T>::value>
{};


template <typename T>
inline void tag_move_if(T& lhs, T&& rhs, std::true_type)
{ lhs = std::move(rhs); }

template <typename T>
inline void tag_move_if(T&, T&&, std::false_type) {}

template <typename T>
inline void tag_copy_if(T& lhs, const T& rhs, std::true_type)
{ lhs = rhs; }

template <typename T>
inline void tag_copy_if(T&, const T&, std::false_type) {}


// Absolute value of a signed integral type. Converts to unsigned equivalent.
template <typename T>
constexpr make_unsigned_t<T> absu(T value) noexcept
{
    // are there any systems that would violate this?
    static_assert(
        std::numeric_limits<T>::max() + std::numeric_limits<T>::min() == 0 ||
        // can unsigned T store |signed min T|?
        std::numeric_limits<make_unsigned_t<T>>::digits > std::numeric_limits<T>::digits,
        "Platform not supported.");

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4146) // unary minus operator applied to unsigned type
#endif
    return value < 0 ? -static_cast<make_unsigned_t<T>>(value) : value;
#ifdef _MSC_VER
#pragma warning(pop)
#endif
}

// Negate an unsigned integral type. Converts to signed equivalent.
// The behavior is undefined if the value is not convertible.
template <typename T,
    T lbound = std::numeric_limits<T>::min(),
    T ubound = std::numeric_limits<T>::max()>
inline
#ifdef NDEBUG
constexpr
#endif
T uneg(make_unsigned_t<T> uvalue) noexcept
{
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4146) // unary minus operator applied to unsigned type
#endif
    static_assert(lbound < 0 && ubound > 0 &&
        absu(lbound) >= ubound, "Invalid lower or upper bound.");
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#ifndef NDEBUG
    assert(uvalue <= absu(lbound));
#endif
    return uvalue <= ubound ? -static_cast<T>(uvalue) :
        -ubound - static_cast<T>(uvalue - ubound);
}

// Portably convert an unsigned integral type to its signed equivalent.
// Values larger than signed max wrap around to negative values.
template <typename T>
constexpr make_signed_t<T> utos(T value) noexcept
{
    // Can signed T represent all values of unsigned T after wrap-around?
    static_assert(std::numeric_limits<make_signed_t<T>>::max() + 
        absu(std::numeric_limits<make_signed_t<T>>::min()) >= std::numeric_limits<T>::max(),
        "Platform not supported.");

    return value > std::numeric_limits<make_signed_t<T>>::max() ?
        // https://stackoverflow.com/questions/13150449/
        static_cast<make_signed_t<T>>(value - 
            absu(std::numeric_limits<make_signed_t<T>>::min())) + std::numeric_limits<make_signed_t<T>>::min() : 
        static_cast<make_signed_t<T>>(value);
}

// Round an unsigned value up to the closest multiple of factor.
template <typename T>
constexpr T uround_up(T value, T factor)
{
    static_assert(std::is_unsigned<T>::value, "T must be unsigned");
    return value != 0 && factor != 0 ? (value + factor - 1) - (value + factor - 1) % factor : 0;
}


template <typename CharT>
inline bool is_digit(CharT c) noexcept { return c >= 0x30 && c <= 0x39; }

template <typename CharT>
inline bool is_ws(CharT c) noexcept
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

template <typename CharT>
inline CharT to_lower(CharT c) noexcept { return c >= 0x41 && c <= 0x5a ? c + 32 : c; }

template <typename ConstIterator, typename CharT, typename Pred>
inline bool starts_with(ConstIterator str_begin, ConstIterator str_end, 
    const CharT* prefix, std::size_t prefix_size, Pred is_equal)
{
    assert(str_end >= str_begin);

    auto strp = str_begin;
    for (std::size_t i = 0; i < prefix_size; ++i)
    {
        if (strp == str_end) return false;
        if (!is_equal(*strp, prefix[i])) return false;
        strp++;
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

// True if T is a type designated to access raw memory.
template <typename T>
using is_byte_like = std::integral_constant<bool,
#if SIJSON_HAS_STDBYTE
    std::is_same<T, std::byte>::value ||
#endif
    std::is_same<T, unsigned char>::value ||
    std::is_same<T, char>::value
>;

// True if T may be used for UTF-8 character representation.
template <typename T>
using is_char8_like = std::integral_constant<bool,
#ifdef __cpp_char8_t
    std::is_same<T, char8_t>::value ||
#endif
    std::is_same<T, unsigned char>::value ||
    std::is_same<T, char>::value
>;




inline std::runtime_error parse_error(std::size_t pos, const std::string& message)
{
    return std::runtime_error("Parse error at offset " + std::to_string(pos) + ": " + message);
}
inline std::runtime_error parse_error(std::size_t pos, const char* message)
{
    return std::runtime_error("Parse error at offset " + std::to_string(pos) + ": " + message);
}
inline std::runtime_error parse_error_exp(std::size_t pos, const std::string& expected)
{
    return parse_error(pos, "expected " + expected);
}
inline std::runtime_error parse_error_exp(std::size_t pos, const char* message)
{
    return std::runtime_error("Parse error at offset " + std::to_string(pos) + ": expected " + message);
}
}}

// Utility functions/definitions (internal use only).
namespace iutil = internal::util;
}

#endif