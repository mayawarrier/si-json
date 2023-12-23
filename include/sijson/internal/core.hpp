
#ifndef SIJSON_CORE_HPP
#define SIJSON_CORE_HPP

#include <cstddef>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <limits>
#include <memory>
#include <utility>
#include <algorithm>
#include <string>
#include <iostream>
#include <type_traits>
#include <stdexcept>
#include <new>

#include "config.hpp"

#if SIJSON_HAS_INCLUDE(<stdfloat>)
#include <stdfloat>
#endif

#ifdef SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif

#ifdef SIJSON_NO_MAGIC_STATICS
#include <mutex>
#endif

#ifndef SIJSON_HAS_STRTOFP_L
// fallback
#include <locale>
#include <clocale>
#endif

#if SIJSON_EXC_STACKTRACE
#if !defined(SIJSON_HAS_STACKTRACE)
#error "Cannot find stacktrace implementation."
#else
#include <stacktrace>
#endif
#endif


namespace sijson {

enum token : unsigned
{
    TOKEN_start_object,
    TOKEN_end_object,
    TOKEN_start_array,
    TOKEN_end_array,
    TOKEN_key_separator,
    TOKEN_item_separator,
    TOKEN_number,
    TOKEN_string,
    TOKEN_boolean,
    TOKEN_null,
    // end of input
    TOKEN_eof,
    // invalid token
    TOKEN_invalid
};

enum endian
{
    ENDIAN_little,
    ENDIAN_big
};

enum encoding
{
    ENCODING_utf8,
    ENCODING_utf16,
    ENCODING_utf32,
    ENCODING_utf16le,
    ENCODING_utf16be,
    ENCODING_utf32le,
    ENCODING_utf32be
};

enum docnode : unsigned
{
    DOCNODE_array,
    DOCNODE_object,
    DOCNODE_key,
    DOCNODE_value,
    DOCNODE_root
};
// Number of docnode types.
static constexpr int NUM_DOCNODES = 5;

inline const char* docnode_name(docnode type)
{
    switch (type)
    {
    case DOCNODE_array: return "array";
    case DOCNODE_object: return "object";
    case DOCNODE_key: return "key";
    case DOCNODE_value: return "value";
    case DOCNODE_root: return "root";
    default: return "Invalid DOCNODE";
    }
}

// always >= 0
enum error : int
{
    ERROR_none = 0,
    ERROR_out_of_range,
    ERROR_invalid_num,
    ERROR_bad_rdflags,
    ERROR_str_delim,
    ERROR_str_escape,
    ERROR_token_start_object,
    ERROR_token_end_object,
    ERROR_token_start_array,
    ERROR_token_end_array,
    ERROR_token_item_sep,
    ERROR_token_key_sep,
    ERROR_token_bool,
    ERROR_token_null
};

inline const char* error_msg(error e)
{
    switch (e)
    {
    case ERROR_none:               return "No error.";
    case ERROR_out_of_range:       return "Out of range.";
    case ERROR_invalid_num:        return "Invalid number.";
    case ERROR_bad_rdflags:        return "Invalid read flag(s).";
    case ERROR_str_delim:          return "String delimiter missing.";
    case ERROR_str_escape:         return "Invalid string escape.";
    case ERROR_token_start_object: return "Expected '{'";
    case ERROR_token_end_object:   return "Expected '}'";
    case ERROR_token_start_array:  return "Expected '['";
    case ERROR_token_end_array:    return "Expected ']'";
    case ERROR_token_item_sep:     return "Expected ','";
    case ERROR_token_key_sep:      return "Expected ':'";
    case ERROR_token_bool:         return "Expected boolean.";
    case ERROR_token_null:         return "Expected null.";
    default:                       return "Unknown error.";
    }
}

class parse_error : public std::runtime_error
{
public:
    template <typename OffT>
    parse_error(OffT offset, const char* msg) :
        std::runtime_error(get_msg(offset, msg))
    {}

    template <typename OffT>
    parse_error(OffT offset, const std::string& msg) :
        std::runtime_error(get_msg(offset, msg.c_str()))
    {}

    template <typename OffT>
    parse_error(OffT offset, error e) :
        std::runtime_error(get_msg(offset, error_msg(e)))
    {}

private:
    template <typename OffT>
    static inline std::string get_msg(OffT off, const char* msg)
    {
        auto res = "JSON parse error at offset " + std::to_string(off) + ": " + msg;
#if SIJSON_EXC_STACKTRACE
        res += "\nStack trace:\n" + std::to_string(std::stacktrace::current());
#endif
        return res;
    }
};

// Implements the basic I/O interface (see BasicInput/BasicOutput in concepts.hpp).
struct io_basic { static constexpr unsigned flags = 0x1; };

// Implements the contiguous I/O interface (see ContiguousInput/ContiguousOutput in concepts.hpp).
struct io_contiguous { static constexpr unsigned flags = 0x2 | 0x1; };

// Option: null-terminate this string stream.
struct null_terminate { static constexpr int id = 1; };

// Option: For readers, assume no whitespace is present in the input.
struct no_whitespace { static constexpr int id = 2; };

// Option: throw if an operation will result in buffer overflow.
struct throw_on_overflow { static constexpr int id = 3; };


template <encoding Encoding>
struct encoding_traits {};

template <>
struct encoding_traits<ENCODING_utf8>
{
#ifdef __cpp_char8_t
    using char_type = char8_t;
#else
    using char_type = char;
#endif
};

template <> struct encoding_traits<ENCODING_utf16> { using char_type = char16_t; };
template <> struct encoding_traits<ENCODING_utf16le> { using char_type = char16_t; };
template <> struct encoding_traits<ENCODING_utf16be> { using char_type = char16_t; };

template <> struct encoding_traits<ENCODING_utf32> { using char_type = char32_t; };
template <> struct encoding_traits<ENCODING_utf32le> { using char_type = char32_t; };
template <> struct encoding_traits<ENCODING_utf32be> { using char_type = char32_t; };


//template <typename CharT>
//struct encoded_char_traits {};
//
//template <> struct encoded_char_traits<char> { static constexpr encoding encoding = ENCODING_utf8; };
//template <> struct encoded_char_traits<char16_t> { static constexpr encoding encoding = ENCODING_utf16; };
//template <> struct encoded_char_traits<char32_t> { static constexpr encoding encoding = ENCODING_utf32; };
//
//#ifdef __cpp_char8_t
//template <> struct encoded_char_traits<char8_t> { static constexpr encoding encoding = ENCODING_utf8; };
//#endif


namespace internal {
namespace util {}
}
namespace iutil = internal::util;

namespace internal {
// Data
namespace util {

static constexpr auto int32_max = 0x7FFFFFFF;
static constexpr auto int32_min = -0x7FFFFFFF - 1;
static constexpr auto uint32_max = 0xFFFFFFFF;

static constexpr auto int64_max = 0x7FFFFFFFFFFFFFFF;
static constexpr auto int64_min = -0x7FFFFFFFFFFFFFFF - 1;
static constexpr auto uint64_max = 0xFFFFFFFFFFFFFFFF;

template <typename CharT>
struct strings
{
    static constexpr CharT null_str[] = { CharT('n'), CharT('u'), CharT('l'), CharT('l') };
    static constexpr CharT false_str[] = { CharT('f'), CharT('a'), CharT('l'), CharT('s'), CharT('e') };
    static constexpr CharT true_str[] = { CharT('t'), CharT('r'), CharT('u'), CharT('e') };
};

template <typename = void>
struct LUTS
{
    // row = 16 chars
    // \b, \f, \n, \r, \t, /, \, "
    static constexpr char ctrl[] = 
    {
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0, '"', 0,0,0,0,0,0,0,0,0,0,0,0, '/',
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0, '\\', 0,0,0,
        0,0, '\b', 0,0,0, '\f', 0,0,0,0,0,0,0, '\n', 0,
        0,0, '\r', 0, '\t', 0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    };

    // row = 32 chars
    // \t, \n, \r, ' '
    static constexpr bool ws[] = 
    {
        0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    };
};

template <typename T>
constexpr char LUTS<T>::ctrl[];

template <typename T>
constexpr bool LUTS<T>::ws[];

template <typename CharT>
constexpr char ctrl_lut(CharT c) 
{
    using uchar = unsigned char;
    return c >= 0 && c <= 255 ? LUTS<>::ctrl[uchar(c)] : 0;
}

template <typename CharT>
constexpr bool is_ws(CharT c) noexcept
{
    using uchar = unsigned char;
    return c >= 0 && c <= 255 ? LUTS<>::ws[uchar(c)] : false;
    // faster? https://pdimov.github.io/blog/2020/07/19/llvm-and-memchr/
    // return c <= static_cast<CharT>(32) && ((1ull << c) & 0x100002600ull);
}
}

// Low-level utils
namespace util {

template <bool test, typename T = int>
using enable_if_t = typename std::enable_if<test, T>::type;

template <typename A, typename B, typename T = int>
using enable_if_same_t = iutil::enable_if_t<std::is_same<A, B>::value, T>;

#if defined (__cpp_lib_logical_traits) || SIJSON_CPLUSPLUS >= 201703L
using std::conjunction;
using std::disjunction;
using std::negation;
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


#if (defined(__cpp_lib_void_t) || SIJSON_CPLUSPLUS >= 201703L) && \
    _MSC_VER >= 1920 // void_t<noexcept(...)> is broken pre-VS2019
using std::void_t;
#else
// https://en.cppreference.com/w/cpp/types/void_t
// https://cplusplus.github.io/CWG/issues/1558.html
template <typename ...Ts> struct make_void { using type = void; };
template <typename ...Ts> using void_t = typename make_void<Ts...>::type;
#endif


#ifdef __cpp_lib_remove_cvref
using std::remove_cvref_t;
#else
template <typename T>
using remove_cvref_t = typename std::remove_cv<
    typename std::remove_reference<T>::type>::type;
#endif

template <typename T>
using remove_reference_t = typename std::remove_reference<T>::type;

template <typename T>
using decay_t = typename std::decay<T>::type;

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
    std::is_standard_layout<T>::value&&
    std::is_trivial<T>::value&&
    std::is_trivially_destructible<T>::value>;

template <typename T, template <typename, typename...> class U>
struct is_instance_of : std::false_type {};

template <typename ...Args, template <typename, typename...> class U>
struct is_instance_of<U<Args...>, U> : std::true_type {};


template <typename T, typename ...Ts>
using is_any_of = iutil::disjunction<std::is_same<T, Ts>...>;


template <typename T, bool Enable>
struct add_const_if { using type = T; };

template <typename T>
struct add_const_if<T, true> { using type = const T; };

template <typename T, bool Enable>
using add_const_if_t = typename add_const_if<T, Enable>::type;


template <std::size_t ...Vals>
struct size_t_max;

template <>
struct size_t_max<> : std::integral_constant<std::size_t, 0> {};

template <std::size_t Val>
struct size_t_max<Val> : std::integral_constant<std::size_t, Val> {};

template <std::size_t Lhs, std::size_t Rhs, std::size_t ...Rest>
struct size_t_max<Lhs, Rhs, Rest...> : size_t_max<(Lhs > Rhs ? Lhs : Rhs), Rest...>::type
{};

// Type with largest size.
template <typename ...Ts>
constexpr std::size_t max_size(void) { return size_t_max<sizeof(Ts)...>::value; }

// https://en.cppreference.com/w/cpp/utility/in_place
struct in_place_t
{
    constexpr explicit in_place_t() = default;
};
static constexpr in_place_t in_place{};

// Empty placeholder class.
// Workaround for std::allocator<void> deprecation warnings 
// even though it only exists as a placeholder for rebind
struct empty {};

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

#ifdef SIJSON_HAS_STRING_VIEW
// True if T is basic_string_view<CharT, ...>.
template <typename T, typename CharT>
using is_instance_of_basic_string_view = conjunction<
    is_instance_of<T, std::basic_string_view>, has_value_type<T, CharT>>;
#endif

template <typename T, typename CharT>
using is_writable_string_type = iutil::disjunction<
    std::is_same<typename std::decay<T>::type, CharT*>,
    std::is_same<typename std::decay<T>::type, const CharT*>,
    iutil::is_instance_of_basic_string<T, CharT>
#ifdef SIJSON_HAS_STRING_VIEW
    , iutil::is_instance_of_basic_string_view<T, CharT>
#endif
>;

template <typename T, typename CharT>
using is_readable_string_type = iutil::is_instance_of_basic_string<T, CharT>;


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

// Simplify instantiation of std::basic_string<> without custom traits.
template <typename CharT, typename Allocator>
using stdstr = std::basic_string<CharT, std::char_traits<CharT>, Allocator>;


// Size of buffer large enough to store a number as decimal text with round-trip guarantee (includes sign).
template <typename T, bool = std::is_floating_point<T>::value>
struct max_outchars10 : std::integral_constant<int,
    // max precision + exponent (any standard fp type can't have an
    // exponent larger than int, see min_exponent) + sign + point + e
    std::numeric_limits<T>::max_digits10 + (std::numeric_limits<int>::digits10 + 2) + 3>
{};

template <typename T>
struct max_outchars10<T, false> : std::integral_constant<int,
    // round up digits10 + sign
    std::numeric_limits<T>::digits10 + 1 + std::is_signed<T>::value>
{};

template <typename T>
inline void tag_move_if(T& lhs, T&& rhs, std::true_type) { lhs = std::move(rhs); }

template <typename T>
inline void tag_move_if(T&, T&&, std::false_type) {}

template <typename T>
inline void tag_copy_if(T& lhs, const T& rhs, std::true_type) { lhs = rhs; }

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
#pragma warning(disable: 4146) 
#endif
    return value < 0 ? -static_cast<make_unsigned_t<T>>(value) : value;
#ifdef _MSC_VER
#pragma warning(pop)
#endif
}

// Negate an unsigned integral type and convert to signed.
// Behavior is undefined unless lbound < 0, ubound > 0, 
// absu(lbound) >= bound, and uvalue <= absu(lbound).
template <typename T,
    T lbound = std::numeric_limits<T>::min(),
    T ubound = std::numeric_limits<T>::max()>
constexpr T uneg(make_unsigned_t<T> uvalue) noexcept
{
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4146) 
#endif
    static_assert(lbound < 0 && ubound > 0 &&
        absu(lbound) >= ubound, "Invalid lower or upper bound.");
#ifdef _MSC_VER
#pragma warning(pop)
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
        static_cast<make_signed_t<T>>(value - absu(std::numeric_limits<make_signed_t<T>>::min())) +
        std::numeric_limits<make_signed_t<T>>::min() :
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
constexpr bool is_digit(CharT c) noexcept { return c >= CharT('0') && c <= CharT('9'); }

template <typename CharT>
inline CharT to_lower(CharT c) noexcept { return c >= 0x41 && c <= 0x5a ? c + 32 : c; }



template <typename T>
struct to_basicfp { using type = T; };

#ifdef __STDCPP_FLOAT32_T__
template <>
struct to_basicfp<std::float32_t> { using type = float; };
#endif
#ifdef __STDCPP_FLOAT64_T__
template <>
struct to_basicfp<std::float64_t> { using type = double; };
#endif

template <typename T>
using to_basicfp_t = typename to_basicfp<T>::type;


#if SIJSON_USE_FASTFLOAT

template <typename T>
struct to_ff_compat { using type = to_basicfp_t<T>; };

template <>
struct to_ff_compat<long double> { using type = double; };

template <typename T>
using to_ff_compat_t = typename to_ff_compat<T>::type;

#endif

// thread-local fp buffer
template <typename = void>
struct TLB_FP
{
    // https://www.exploringbinary.com/maximum-number-of-decimal-digits-in-binary-floating-point-numbers/
    // Should be 16494 for fp128, a little extra just in case
    static thread_local char BUF[16594];
};
template <typename T>
thread_local char TLB_FP<T>::BUF[];


template <typename SiInput>
inline bool take(SiInput& is, typename SiInput::char_type c)
{
    if (!is.end() && is.peek() == c)
    {
        is.take();
        return true;
    }
    else return false;
}

// True if T is a type designated to access raw memory.
template <typename T>
using is_byte_like = std::integral_constant<bool,
#ifdef SIJSON_HAS_STDBYTE
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


#ifdef __cpp_lib_to_address
using std::to_address;
#else

// https://en.cppreference.com/w/cpp/memory/to_address, modified for C++11

template <typename Ptr, typename = void>
struct ptr_traits_has_to_address : std::false_type {};

template <typename Ptr>
struct ptr_traits_has_to_address<Ptr,
    void_t<decltype(std::pointer_traits<Ptr>::to_address(std::declval<const Ptr&>()))>> :
    std::true_type
{};

template <typename T>
constexpr T* to_address(T* p) noexcept
{
    static_assert(!std::is_function<T>::value, "T cannot be a function type.");
    return p;
}

template <typename Ptr,
    iutil::enable_if_t<ptr_traits_has_to_address<Ptr>::value> = 0>
inline typename std::pointer_traits<Ptr>::element_type* to_address(const Ptr& p) noexcept
{
    return std::pointer_traits<Ptr>::to_address(p);
}

template <typename Ptr,
    iutil::enable_if_t<!ptr_traits_has_to_address<Ptr>::value> = 0>
inline auto to_address(const Ptr& p) noexcept -> decltype(iutil::to_address(p.operator->()))
{
    return iutil::to_address(p.operator->());
}
#endif

template <typename T>
SIJSON_CONSTEXPR20 void destroy_at(T* p)
{
    static_assert(!std::is_array<T>::value, "T cannot be an array type.");
    p->~T();
}

// https://en.cppreference.com/w/cpp/compiler_support/17
// available in MSVC STL from VS 2017 15.7
// available in libc++ from 6.0
// clang, gcc have a builtin (__builtin_launder)
// 
// might be necessary for these edge cases (todo):
// https://miyuki.github.io/2016/10/21/std-launder.html,
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2017/p0532r0.pdf, pg 6)
//
#if __cpp_lib_launder >= 201606L || \
    (_MSC_FULL_VER >= 191426428 && SIJSON_CPLUSPLUS >= 201703L) || \
    (_LIBCPP_VERSION >= 6000 && _LIBCPP_STD_VER > 14)

#define SIJSON_HAS_LAUNDER 1
using std::launder;

#elif SIJSON_HAS_BUILTIN(__builtin_launder)
#define SIJSON_HAS_LAUNDER 1

template <typename T>
constexpr T* launder(T* p) noexcept
{
    static_assert(!std::is_function<T>::value && !std::is_void<T>::value,
        "T cannot be a function type or cv-void.");
    return __builtin_launder(p);
}
#endif


// Provides properly aligned POD storage for T.
template <typename T>
class aligned_storage_for
{
#if SIJSON_LAUNDER_ALIGNED_STORAGE && !defined(SIJSON_HAS_LAUNDER)
#error "aligned_storage_for<T> needs launder implementation."
#endif

public:
    template <typename ...Args>
    inline void construct(Args&&... args)
    {
        ::new (&m_buf) T(std::forward<Args>(args)...);
    }

    void destroy(void) { iutil::destroy_at(ptr()); }

    inline T* ptr(void) noexcept
    {
SIJSON_IGNORE_WCAST_ALIGN
#if SIJSON_LAUNDER_ALIGNED_STORAGE
        return iutil::launder(reinterpret_cast<T*>(&m_buf));
#else
        return reinterpret_cast<T*>(&m_buf);
#endif
SIJSON_RESTORE_WCAST_ALIGN
    }

    inline const T* ptr(void) const noexcept
    {
SIJSON_IGNORE_WCAST_ALIGN
#if SIJSON_LAUNDER_ALIGNED_STORAGE
        return iutil::launder(reinterpret_cast<const T*>(&m_buf));
#else
        return reinterpret_cast<const T*>(&m_buf);
#endif
SIJSON_RESTORE_WCAST_ALIGN
    }

    inline T& get(void) noexcept { return *ptr(); }
    inline const T& get(void) const noexcept { return *ptr(); }

private:
    alignas(T) unsigned char m_buf[sizeof(T)];
};


// Rebind an allocator to a different value_type.
template <typename Allocator, typename T>
using rebind_alloc_t = typename std::allocator_traits<Allocator>::template rebind_alloc<T>;

template <typename Allocator>
using alloc_ptr_t = typename std::allocator_traits<Allocator>::pointer;

template <typename Allocator>
using alloc_cptr_t = typename std::allocator_traits<Allocator>::const_pointer;


template <typename T, typename = void>
struct alloc_is_always_equal : std::is_empty<T> {};

// added in a C++11 defect report, check if present
template <typename T>
struct alloc_is_always_equal<T, void_t<
    typename std::allocator_traits<T>::is_always_equal>> :
    std::allocator_traits<T>::is_always_equal
{};

template <typename, typename...>
struct alloc_has_custom_construct : std::false_type {};

template <typename Allocator, typename T, typename ...CtorArgs>
struct alloc_has_custom_construct<iutil::void_t<
    decltype(std::declval<Allocator&>().construct(std::declval<T*>(), std::declval<CtorArgs>()...))>,
    Allocator, T*, CtorArgs...> : std::true_type
{};

template <typename Allocator, typename T = typename Allocator::value_type, typename ...CtorArgs>
using alloc_construct_is_noexcept = std::integral_constant<bool,
    noexcept(std::declval<Allocator&>().construct(std::declval<T*>(), std::declval<CtorArgs>()...))>;

template <typename, typename, typename = void>
struct alloc_has_custom_destroy : std::false_type {};

template <typename Allocator, typename T>
struct alloc_has_custom_destroy<Allocator, T, iutil::void_t<
    decltype(std::declval<Allocator&>().destroy(std::declval<T*>()))>> : std::true_type
{};

// todo: can optimize for polymorphic_allocator and scoped_allocator_adaptor as well, but 
// the way they handle std::pair is nightmarish (to say the least), and it would be difficult
// to work around

// True if T must be default-constructed for a given allocator.
template <typename Allocator, typename T = typename Allocator::value_type>
using alloc_must_default_construct = std::integral_constant<bool, !std::is_trivially_constructible<T>::value ||
    conjunction<negation<is_instance_of<Allocator, std::allocator>>, alloc_has_custom_construct<Allocator, T>>::value>;

// True if T must be destructed for a given allocator.
template <typename Allocator, typename T = typename Allocator::value_type>
using alloc_must_destroy = std::integral_constant<bool, !std::is_trivially_destructible<T>::value ||
    conjunction<negation<is_instance_of<Allocator, std::allocator>>, alloc_has_custom_destroy<Allocator, T>>::value>;


// Equivalent to new for custom allocators.
// Strong exception guarantee if alloc.destroy() is noexcept.
template <typename Allocator>
inline alloc_ptr_t<Allocator> alloc_new(Allocator& alloc, std::size_t size)
{
    using Altraits = std::allocator_traits<Allocator>;

    auto p = Altraits::allocate(alloc, size);
    if (alloc_must_default_construct<Allocator>::value)
    {
        //if (alloc_construct_is_noexcept<Allocator>::value)

        std::size_t i;
        try {
            for (i = 0; i < size; ++i)
                Altraits::construct(alloc, iutil::to_address(p) + i);
        }
        catch (...) {
            if (alloc_must_destroy<Allocator>::value)
                for (std::size_t j = 0; j < i; ++j)
                    Altraits::destroy(alloc, iutil::to_address(p) + j);

            Altraits::deallocate(alloc, p, size);
            throw;
        }
    }

    return p;
}

// alloc_delete() but does not check for null.
// Noexcept if alloc.destroy() is noexcept.
template <typename Allocator>
inline void alloc_unchecked_delete(Allocator& alloc, alloc_ptr_t<Allocator> p, std::size_t size)
{
    using Altraits = std::allocator_traits<Allocator>;

    if (alloc_must_destroy<Allocator>::value)
        for (std::size_t i = 0; i < size; ++i)
            Altraits::destroy(alloc, iutil::to_address(p) + i);

    Altraits::deallocate(alloc, p, size);
}

// Equivalent to delete for custom allocators.
// Noexcept if alloc.destroy() is noexcept.
template <typename Allocator>
inline void alloc_delete(Allocator& alloc, alloc_ptr_t<Allocator> ptr, std::size_t size)
{
    if (ptr) alloc_unchecked_delete(alloc, ptr, size);
}


// Align allocation size to 'alignment' bytes.
template <typename T, std::size_t alignment = 16, typename Allocator>
inline std::size_t align_allocn_size(Allocator& alloc, std::size_t size)
{
    auto max_size = std::allocator_traits<Allocator>::max_size(alloc);

    static constexpr auto my_alignment =
        sizeof(T) < alignment ? alignment / sizeof(T) : 1;

    if (size > max_size - my_alignment + 1)
        return max_size;
    auto aligned = (size + my_alignment - 1) & ~(my_alignment - 1);

    if (aligned > max_size)
        return max_size;

    return aligned;
}

// Recommend an allocation size for a vector-like container.
template <typename T, typename Allocator>
inline std::size_t vector_allocn_size(Allocator& alloc, std::size_t old_size, std::size_t new_size)
{
    auto max_size = std::allocator_traits<Allocator>::max_size(alloc);
    return old_size <= max_size / 2 ?
        align_allocn_size<T>(alloc, std::max(new_size, 2 * old_size)) : max_size;
}

template <typename Allocator>
class scope_guard
{
public:
    scope_guard(Allocator& alloc, alloc_ptr_t<Allocator> data, std::size_t size) noexcept :
        m_alloc(alloc), m_data(data), m_size(size), m_enabled(true)
    {}

    inline void disable(void) noexcept { m_enabled = false; }

    ~scope_guard()
    {
        if (m_enabled)
            alloc_delete(m_alloc, m_data, m_size);
    }
private:
    Allocator& m_alloc;
    alloc_ptr_t<Allocator> m_data;
    std::size_t m_size;
    bool m_enabled;
};


template <typename T>
inline void move_alloc_if_pocma(T& lhs, T&& rhs) noexcept
{
    tag_move_if(lhs, std::move(rhs), std::integral_constant<bool,
        std::allocator_traits<T>::propagate_on_container_move_assignment::value>{});
}

template <typename T>
inline void copy_alloc_if_pocca(T& lhs, const T& rhs) noexcept
{
    tag_copy_if(lhs, rhs, std::integral_constant<bool,
        std::allocator_traits<T>::propagate_on_container_copy_assignment::value>{});
}

template <typename T,
    bool EnableEbcoFallback = false,
#if defined(__cpp_lib_is_final) || SIJSON_CPLUSPLUS >= 201402L
    bool = std::is_empty<T>::value && !std::is_final<T>::value
#else
    bool = EnableEbcoFallback
#endif
>
class ebco : private T
{
public:
    template <typename ...Args>
    explicit ebco(iutil::in_place_t, Args&&... args) :
        T(std::forward<Args>(args)...)
    {}

    inline T& ebco_v(void) noexcept { return *this; }
    inline const T& ebco_v(void) const noexcept { return *this; }
};

template <typename T, bool EnableEbcoFallback>
class ebco<T, EnableEbcoFallback, false>
{
public:
    template <typename ...Args>
    explicit ebco(iutil::in_place_t, Args&&... args) :
        m_value(std::forward<Args>(args)...)
    {}

    inline T& ebco_v(void) noexcept { return m_value; }
    inline const T& ebco_v(void) const noexcept { return m_value; }

private:
    T m_value;
};


// Wraps an allocator that implements std::allocator_traits.
// Tries to apply EBCO on the allocator type.
// See https://en.cppreference.com/w/cpp/language/ebo.
template <typename T>
class alloc_holder : private ebco<T, is_instance_of<T, std::allocator>::value>
{
private:
    using base = ebco<T, is_instance_of<T, std::allocator>::value>;

public:
    template <typename ...Args>
    explicit alloc_holder(iutil::in_place_t, Args&&... args)
        noexcept(noexcept(T(std::forward<Args>(args)...))) :
        base(iutil::in_place, std::forward<Args>(args)...)
    {}

    alloc_holder(alloc_holder&& rhs) noexcept = default;

    alloc_holder(const alloc_holder& rhs) noexcept :
        base(iutil::in_place,
            std::allocator_traits<T>::select_on_container_copy_construction(rhs.alloc()))
    {}

    inline alloc_holder& operator=(alloc_holder&& rhs) noexcept
    {
        if (this != &rhs)
            move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        return *this;
    }
    inline alloc_holder& operator=(const alloc_holder& rhs) noexcept
    {
        if (this != &rhs)
            copy_alloc_if_pocca(this->alloc(), rhs.alloc());
        return *this;
    }

    inline T& alloc(void) noexcept { return this->ebco_v(); }
    inline const T& alloc(void) const noexcept { return this->ebco_v(); }
};

template <typename T>
constexpr bool alloc_always_equal_after_move(void) noexcept
{
    return alloc_is_always_equal<T>::value ||
        std::allocator_traits<T>::propagate_on_container_move_assignment::value;
}
}

// Concepts
namespace util {

template <typename T, typename CharT>
using check_io_typedefs = iutil::void_t<
    iutil::enable_if_same_t<typename T::char_type, CharT>,
    iutil::enable_if_t<iutil::is_nb_unsigned_integral<typename T::size_type>::value>>;


template <typename T, typename CharT>
using is_basic_input_impl = iutil::void_t<
    check_io_typedefs<T, CharT>,
    iutil::enable_if_t<(T::input_kind::flags & 0x1u) != 0>,
    iutil::enable_if_same_t<decltype(std::declval<T>().peek()), CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().take()), CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().ipos()), typename T::size_type>,
    iutil::enable_if_same_t<decltype(std::declval<T>().end()), bool>,
    decltype(std::declval<T>().rewind())>;


template <typename T, typename CharT>
using is_nothrow_basic_input_impl = std::integral_constant<bool,
    noexcept(std::declval<T>().peek()) &&
    noexcept(std::declval<T>().take()) &&
    noexcept(std::declval<T>().ipos()) &&
    noexcept(std::declval<T>().end()) &&
    noexcept(std::declval<T>().rewind())>;


template <typename T, typename CharT>
using is_basic_output_impl = iutil::void_t<
    check_io_typedefs<T, CharT>,
    iutil::enable_if_t<(T::output_kind::flags & 0x1u) != 0>,
    decltype(std::declval<T>().put(std::declval<CharT>())),
    decltype(std::declval<T>().put_f(std::declval<CharT>(), std::declval<typename T::size_type>())),
    decltype(std::declval<T>().put_n(std::declval<const CharT*>(), std::declval<std::size_t>())),
    iutil::enable_if_same_t<decltype(std::declval<T>().opos()), typename T::size_type>,
    decltype(std::declval<T>().flush())>;


template <typename T, typename CharT>
using is_nothrow_basic_output_impl = std::integral_constant<bool,
    noexcept(std::declval<T>().put(std::declval<CharT>())) &&
    noexcept(std::declval<T>().put_f(std::declval<CharT>(), std::declval<typename T::size_type>())) &&
    noexcept(std::declval<T>().put_n(std::declval<const CharT*>(), std::declval<std::size_t>())) &&
    noexcept(std::declval<T>().opos()) &&
    noexcept(std::declval<T>().flush())>;


template <typename T, typename CharT>
using is_contiguous_input_impl = iutil::void_t<
    is_basic_input_impl<T, CharT>,
    iutil::enable_if_t<(T::input_kind::flags & 0x2u) != 0>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().ipbeg()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().ipend()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().ipcur()), const CharT*>,
    decltype(std::declval<T>().icommit(std::declval<typename T::size_type>()))>;


template <typename T, typename CharT>
using is_nothrow_contiguous_input_impl = std::integral_constant<bool,
    is_nothrow_basic_input_impl<T, CharT>::value &&
    noexcept(std::declval<const T>().ipbeg()) &&
    noexcept(std::declval<const T>().ipend()) &&
    noexcept(std::declval<const T>().ipcur()) &&
    noexcept(std::declval<T>().icommit(std::declval<typename T::size_type>()))>;


template <typename T, typename CharT>
using is_contiguous_output_impl = iutil::void_t<
    is_basic_output_impl<T, CharT>,
    iutil::enable_if_t<(T::output_kind::flags & 0x2u) != 0>,
    decltype(std::declval<T>().reserve(std::declval<typename T::size_type>())),
    iutil::enable_if_same_t<decltype(std::declval<T>().opbeg()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<T>().opend()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<T>().opcur()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().opbeg()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().opend()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().opcur()), const CharT*>,
    decltype(std::declval<T>().ocommit(std::declval<typename T::size_type>()))>;


template <typename T, typename CharT>
using is_nothrow_contiguous_output_impl = std::integral_constant<bool,
    is_nothrow_basic_output_impl<T, CharT>::value &&
    noexcept(std::declval<T>().reserve(std::declval<typename T::size_type>())) &&
    noexcept(std::declval<T>().opbeg()) &&
    noexcept(std::declval<T>().opend()) &&
    noexcept(std::declval<T>().opcur()) &&
    noexcept(std::declval<const T>().opbeg()) &&
    noexcept(std::declval<const T>().opend()) &&
    noexcept(std::declval<const T>().opcur()) &&
    noexcept(std::declval<T>().ocommit(std::declval<typename T::size_type>()))>;


#define SIJSON_GEN_IO_CONCEPT(name, impl) \
template <typename, typename, typename = void> \
struct name : std::false_type {}; \
\
template <typename T, typename CharT> \
struct name<T, CharT, impl<T, CharT>> : \
    std::true_type \
{};

#define SIJSON_GEN_NT_IO_CONCEPT(name, impl, nt_impl) \
template <typename, typename, typename = void> \
struct name : std::false_type {}; \
\
template <typename T, typename CharT> \
struct name<T, CharT, impl<T, CharT>> : \
    nt_impl<T, CharT> \
{};

SIJSON_GEN_IO_CONCEPT(is_basic_input, is_basic_input_impl)
SIJSON_GEN_IO_CONCEPT(is_basic_output, is_basic_output_impl)
SIJSON_GEN_IO_CONCEPT(is_contiguous_input, is_contiguous_input_impl)
SIJSON_GEN_IO_CONCEPT(is_contiguous_output, is_contiguous_output_impl)

SIJSON_GEN_NT_IO_CONCEPT(is_nothrow_basic_input, is_basic_input_impl, is_nothrow_basic_input_impl)
SIJSON_GEN_NT_IO_CONCEPT(is_nothrow_basic_output, is_basic_output_impl, is_nothrow_basic_output_impl)
SIJSON_GEN_NT_IO_CONCEPT(is_nothrow_contiguous_input, is_contiguous_input_impl, is_nothrow_contiguous_input_impl)
SIJSON_GEN_NT_IO_CONCEPT(is_nothrow_contiguous_output, is_contiguous_output_impl, is_nothrow_contiguous_output_impl)

#undef SIJSON_GEN_IO_CONCEPT
#undef SIJSON_GEN_NT_IO_CONCEPT


template <typename, typename, typename = void>
struct is_nothrow_input : std::false_type {};

template <typename T, typename CharT>
struct is_nothrow_input<T, CharT, iutil::enable_if_same_t<
    typename T::input_kind, io_basic, void>> :
    is_nothrow_basic_input<T, CharT>
{};

template <typename T, typename CharT>
struct is_nothrow_input<T, CharT, iutil::enable_if_same_t<
    typename T::input_kind, io_contiguous, void>> :
    is_nothrow_contiguous_input<T, CharT>
{};

template <typename, typename, typename = void>
struct is_nothrow_output : std::false_type {};

template <typename T, typename CharT>
struct is_nothrow_output<T, CharT, iutil::enable_if_same_t<
    typename T::output_kind, io_basic, void>> :
    is_nothrow_basic_output<T, CharT>
{};

template <typename T, typename CharT>
struct is_nothrow_output<T, CharT, iutil::enable_if_same_t<
    typename T::output_kind, io_contiguous, void>> :
    is_nothrow_contiguous_output<T, CharT>
{};
}

// Options 
namespace util {

template <typename ...Opts>
struct opt_list { using type = opt_list<Opts...>; };

template <typename Opt, typename OptList>
struct opt_prepend;

template <typename Opt, typename ...Rest>
struct opt_prepend<Opt, opt_list<Rest...>> : opt_list<Opt, Rest...>
{};

template <typename Opt, typename ...Rest>
struct opt_sort_insert_impl : opt_list<Opt> {};

template <typename A, typename B, typename ...Rest>
struct opt_sort_insert_impl<A, B, Rest...> : opt_prepend<
    typename std::conditional<(A::id < B::id), A, B>::type,
    typename opt_sort_insert_impl< // iterate and place
        typename std::conditional<(A::id > B::id), A, B>::type, Rest...>::type>
{};

template <typename Opt, typename OptList>
struct opt_sort_insert;

template <typename Opt, typename ...Rest>
struct opt_sort_insert<Opt, opt_list<Rest...>> : 
    opt_sort_insert_impl<Opt, Rest...>
{};

// Sorts opt types by id
template <typename OptList>
struct opt_sort : opt_list<> {};

template <typename Opt, typename ...Rest>
struct opt_sort<opt_list<Opt, Rest...>> :
    opt_sort_insert<Opt, typename opt_sort<opt_list<Rest...>>::type>
{};

template <typename Opt, typename ...Opts>
struct has_opt_impl : std::false_type {}; // 0 elements

template <typename Opt, typename Head, typename ...Rest>
struct has_opt_impl<Opt, Head, Rest...> : std::conditional<
    Opt::id == Head::id, std::true_type, // equal?
    has_opt_impl<Opt, Rest...>>::type // iterate
{};

template <typename Opt, typename OptList>
struct has_opt : std::is_same<Opt, OptList> // 1 element or void
{};

template <typename Opt, typename ...Opts>
struct has_opt<Opt, opt_list<Opts...>> : has_opt_impl<Opt, Opts...>
{};
}
}

// A set of compile-time options for an sijson object.
// Two options<...> are guaranteed to be the same type if the
// they contain the same Opts, regardless of order.
// i.e. options<A, B> is the same type as options<B, A>.
template <typename ...Opts>
using options = typename iutil::opt_sort<iutil::opt_list<Opts...>>::type;

// True if the set of options contains opt.
template <typename Options, typename Opt>
constexpr bool has_opt(void) noexcept
{
    return iutil::has_opt<Opt, Options>::value;
}


// Describes a contiguous section of memory.
template <typename T>
struct memspan
{
    constexpr memspan(T* begin, T* end) noexcept :
        begin(begin), end(end)
    {}

    template <typename U,
        iutil::enable_if_t<!std::is_same<U, T>::value && 
        // allow cv conversion eg. memspan<T> --> memspan<const T> implicitly
        // https://stackoverflow.com/questions/42992663/
        std::is_convertible<U(*)[], T(*)[]>::value> = 0
    >
    constexpr memspan(const memspan<U>& rhs) noexcept :
        begin(rhs.begin), end(rhs.end)
    {}

    inline std::size_t size(void) const noexcept
    {
        return (std::size_t)(end - begin);
    }

    T* begin;
    T* end;
};

// Span of the remaining input data i.e. { ipcur(), ipend() }.
template <typename Input>
inline memspan<const typename Input::char_type> indata(const Input& is)
{
    return { is.ipcur(), is.ipend() };
}

// Span of the input area i.e. { ipbeg(), ipend() }.
template <typename Input>
inline memspan<const typename Input::char_type> inarea(const Input& is)
{
    return { is.ipbeg(), is.ipend() };
}

// Num input chars remaining.
template <typename Input>
inline typename Input::size_type inrem(const Input& is)
{
    return is.ipend() - is.ipcur();
}

// Span of the output data { opbeg(), opcur() }.
// Span may be invalidated if a non-const reference to
// the output object is passed to a function or if any non-const
// member functions are called on the output object.
template <typename Output>
inline memspan<iutil::add_const_if_t<typename Output::char_type, std::is_const<Output>::value>>
outdata(Output& os)
{
    return { os.opbeg(), os.opcur() };
}

// Span of the output area { opbeg(), opend() }.
// Span may be invalidated if a non-const reference to
// the output object is passed to a function or if any non-const
// member functions are called on the output object.
template <typename Output>
inline memspan<iutil::add_const_if_t<typename Output::char_type, std::is_const<Output>::value>>
outarea(Output& os)
{
    return { os.opbeg(), os.opend() };
}

enum numtype : int
{
    NUMTYPE_float,
    NUMTYPE_double,
    // Signed integer (stored as int64).
    NUMTYPE_intgr,
    // Unsigned integer (stored as uint64).
    NUMTYPE_uintgr
};

// Holds a numerical value.
class number final
{
public:
    template <typename T,
        iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value> = 0>
    number(T value) noexcept : 
        m_i64(value), m_type(NUMTYPE_intgr)
    {}

    template <typename T,
        iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value> = 0>
    number(T value) noexcept : 
        m_u64(value), m_type(NUMTYPE_uintgr)
    {}

    number(float value) noexcept : 
        m_flt(value), m_type(NUMTYPE_float) 
    {}
    number(double value) noexcept : 
        m_dbl(value), m_type(NUMTYPE_double) 
    {}

    number(void) noexcept : 
        m_dbl(0), m_type(NUMTYPE_double) // largest range
    {} 

    number(number&&) noexcept = default;
    number(const number&) noexcept = default;

    number& operator=(number&&) noexcept = default;
    number& operator=(const number&) noexcept = default;

    // Get type.
    inline numtype type(void) const noexcept { return m_type; }

    // Get value.
    // T must be one of float, double, int64_t or uint64_t.
    // If value is not of type T, behavior is undefined.
    template <typename T>
    inline T get(void) const noexcept;

    // Get value safely.
    // T must be one of float, double, int64_t or uint64_t.
    // If value is not of type T, throws an exception.
    template <typename T>
    inline T get_s(void) const;

    // Get value, converted to given type.
    template <typename T>
    inline T as(void) const noexcept
    {
        switch (m_type)
        {
        case NUMTYPE_float: return T(m_flt);
        case NUMTYPE_double: return T(m_dbl);
        case NUMTYPE_intgr: return T(m_i64);
        case NUMTYPE_uintgr: return T(m_u64);
        default:
#ifndef __cpp_lib_unreachable
            SIJSON_ASSERT(false);
            return {};
#else
            std::unreachable();
#endif
        }
    }
private:
    union
    {
        float m_flt;
        double m_dbl;
        std::int64_t m_i64;
        std::uint64_t m_u64;
    };
    numtype m_type;

    template <typename T>
    struct typehelper
    {
        static constexpr int idx = -1;
    };
};

#define SIJSON_GEN_NUMTYPE_OVERLOAD(type, tidx, memname) \
template <> struct number::typehelper<type> \
{ \
    static constexpr int idx = tidx; \
    static inline type get(const number& n) noexcept { return n.memname; } \
};

// member templates must be at namespace scope
SIJSON_GEN_NUMTYPE_OVERLOAD(float, NUMTYPE_float, m_flt)
SIJSON_GEN_NUMTYPE_OVERLOAD(double, NUMTYPE_double, m_dbl)
SIJSON_GEN_NUMTYPE_OVERLOAD(std::int64_t, NUMTYPE_intgr, m_i64)
SIJSON_GEN_NUMTYPE_OVERLOAD(std::uint64_t, NUMTYPE_uintgr, m_u64)

#undef SIJSON_GEN_NUMTYPE_OVERLOAD

// Get value.
// T must be one of float, double, int64_t or uint64_t.
// If value is not of type T, behavior is undefined.
template <typename T>
inline T number::get(void) const noexcept
{
    return typehelper<T>::get(*this);
}

// Get value safely.
// T must be one of float, double, int64_t or uint64_t.
// If value is not of type T, throws an exception.
template <typename T>
inline T number::get_s(void) const
{
    if (typehelper<T>::idx != m_type)
        throw std::runtime_error(SIJSON_STRFY(sijson::number) "::"
            SIJSON_STRFY(get_s<T>()) ": Type of value is not T.");

    return typehelper<T>::get(*this);
}


namespace internal {
// Buffers
namespace util {

// Dynamically-allocated resizable buffer.
//
// Elements are left uninitialized if alloc.construct() is trivial.
// All member functions have a strong exception guarantee if alloc.destroy() is noexcept.
// 
// https://en.cppreference.com/w/cpp/named_req/Allocator
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p1072r10.html
// 
template <typename T,
    typename Allocator = std::allocator<T>
>
class buffer : private iutil::alloc_holder<Allocator>
{
private:
    using Allocholder = iutil::alloc_holder<Allocator>;
    using Altraits = std::allocator_traits<Allocator>;

public:
    using pointer = typename Altraits::pointer;
    using const_pointer = typename Altraits::const_pointer;
    using iterator = pointer;
    using const_iterator = const_pointer;

public:
    buffer(std::size_t capacity,
        const Allocator& alloc = Allocator()
    ) :
        Allocholder(iutil::in_place, alloc),
        m_size(iutil::align_allocn_size<T>(this->alloc(), capacity)),
        m_bufp(iutil::alloc_new(this->alloc(), m_size))
    {}

    buffer(const Allocator& alloc = Allocator()) :
        buffer(16, alloc)
    {}

    buffer(buffer&& rhs) :
        Allocholder(rhs), m_size(rhs.m_size),
        m_bufp(rhs.m_bufp)
    {
        rhs.m_bufp = nullptr;
    }

    buffer(const buffer& rhs) :
        Allocholder(rhs), m_size(rhs.m_size),
        m_bufp(iutil::alloc_new(this->alloc(), rhs.m_size))
    {
        iutil::scope_guard<Allocator> guard(this->alloc(), m_bufp, m_size);
        std::copy(rhs.m_bufp, rhs.m_bufp + rhs.m_size, m_bufp);
        guard.disable();
    }

    buffer& operator=(const buffer& rhs)
    {
        if (this != &rhs)
        {
            if (Altraits::propagate_on_container_copy_assignment::value)
            {
                if (iutil::alloc_is_always_equal<Allocator>::value ||
                    this->alloc() == rhs.alloc())
                {
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                    copy_assign(rhs, this->alloc());
                }
                else {
                    // else always reallocate (see Allocator requirements)
                    auto alloc = rhs.alloc();
                    copy_assign<true>(rhs, alloc);
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                }
            }
            else copy_assign(rhs, this->alloc());
        }
        return *this;
    }

    buffer& operator=(buffer&& rhs)
    {
        if (this != &rhs)
        {
            if (iutil::alloc_always_equal_after_move<Allocator>() ||
                this->alloc() == rhs.alloc())
            {
                iutil::alloc_unchecked_delete(this->alloc(), m_bufp, m_size);
                // steal
                m_bufp = rhs.m_bufp;
                rhs.m_bufp = nullptr;
                m_size = rhs.m_size;
            }
            // else copy (see Allocator requirements)
            else copy_assign(rhs, this->alloc());

            iutil::move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        }
        return *this;
    }

    inline std::size_t capacity(void) const noexcept { return m_size; }

    inline void reserve(std::size_t new_cap)
    {
        if (new_cap <= capacity())
            return;

        grow_and_copy(this->alloc(), new_cap, m_bufp, m_size);
    }

    inline iterator begin(void) noexcept { return m_bufp; }
    inline iterator end(void) noexcept { return m_bufp + m_size; }

    inline const_iterator begin(void) const noexcept { return m_bufp; }
    inline const_iterator end(void) const noexcept { return m_bufp + m_size; }

    inline T* pbegin(void) noexcept { return iutil::to_address(begin()); }
    inline T* pend(void) noexcept { return iutil::to_address(end()); }

    inline const T* pbegin(void) const noexcept { return iutil::to_address(begin()); }
    inline const T* pend(void) const noexcept { return iutil::to_address(end()); }

    inline T& operator[](std::size_t index) noexcept { return m_bufp[index]; }
    inline const T& operator[](std::size_t index) const noexcept { return m_bufp[index]; }

    inline memspan<T> data(void) noexcept { return { pbegin(), pend() }; }
    inline memspan<const T> data(void) const noexcept { return { pbegin(), pend() }; }

    ~buffer(void)
    {
        iutil::alloc_delete(this->alloc(), m_bufp, m_size);
    }

private:
    // Assumes this != &rhs.
    template <bool MustReallocate = false>
    inline void copy_assign(const buffer& rhs, Allocator& alloc)
    {
        if (MustReallocate || m_size != rhs.m_size)
            reallocate_and_copy(alloc, rhs.m_size, rhs.m_bufp, rhs.m_size);
        else std::copy(rhs.m_bufp, rhs.m_bufp + rhs.m_size, m_bufp);
    }

    inline void reallocate_and_copy(Allocator& alloc, std::size_t new_cap, const_pointer src, std::size_t len)
    {
        SIJSON_ASSERT(new_cap >= len && new_cap != m_size);

        pointer bufp = iutil::alloc_new(alloc, new_cap);
        iutil::scope_guard<Allocator> guard(alloc, bufp, new_cap);

        std::copy(src, src + len, bufp);
        iutil::alloc_unchecked_delete(this->alloc(), m_bufp, m_size);

        m_bufp = bufp;
        m_size = new_cap;

        guard.disable();
    }

    inline void grow_and_copy(Allocator& alloc, std::size_t atleast_cap, const_pointer src, std::size_t len)
    {
        auto new_cap = iutil::vector_allocn_size<T>(alloc, m_size, atleast_cap);
        reallocate_and_copy(alloc, new_cap, src, len);
    }

private:
    std::size_t m_size;
    pointer m_bufp;
};


template <typename T, typename Allocator>
struct strdata_base
{
    using long_pointer_t = iutil::alloc_ptr_t<Allocator>;

    using is_long_pointer_pod = iutil::is_pod<long_pointer_t>;

    using long_pointer_storage_t =
        typename std::conditional<
        is_long_pointer_pod::value,
        long_pointer_t,
        iutil::aligned_storage_for<long_pointer_t>
        >::type;

    struct long_data
    {
        unsigned char is_long : 1;
        std::size_t cap;
        std::size_t len;
        long_pointer_storage_t bufp;
    };

    static constexpr std::size_t SHORT_STRING_CAP =
        (sizeof(long_data) - 1) / sizeof(T) > 1 ?
        (sizeof(long_data) - 1) / sizeof(T) : 1;

    struct short_data
    {
        unsigned char is_long : 1;
        unsigned char len : 7;
        T buf[SHORT_STRING_CAP];
    };

    union data
    {
        long_data long_;
        short_data short_;
    };

    // activate short
    SIJSON_CONSTEXPR20 strdata_base(void) noexcept { m_data.short_.is_long = 0; }

    // Copies object representation only!
    SIJSON_CONSTEXPR20 strdata_base(const data& d) noexcept : m_data(d) {}

    // default would always copy object representation
    strdata_base& operator=(const strdata_base&) = delete;
    strdata_base& operator=(strdata_base&&) = delete;

    // All members must be POD to access is_long from an inactive member
    // https://en.cppreference.com/w/cpp/language/data_members#Standard_layout
    SIJSON_CONSTEXPR20 bool is_long(void) const noexcept { return this->m_data.long_.is_long; }

    SIJSON_CONSTEXPR20 T* short_bufp(void) noexcept { return this->m_data.short_.buf; }
    SIJSON_CONSTEXPR20 const T* short_bufp(void) const noexcept { return this->m_data.short_.buf; }

    SIJSON_CONSTEXPR20 std::size_t long_cap(void) const noexcept { return this->m_data.long_.cap; }
    SIJSON_CONSTEXPR20 std::size_t long_len(void) const noexcept { return this->m_data.long_.len; }

    SIJSON_CONSTEXPR20 void long_cap(std::size_t cap) noexcept { this->m_data.long_.cap = cap; }
    SIJSON_CONSTEXPR20 void long_len(std::size_t len) noexcept { this->m_data.long_.len = len; }

    SIJSON_CONSTEXPR20 void short_len(std::size_t len) noexcept
    {
        SIJSON_ASSERT(len <= this->SHORT_STRING_CAP);
        this->m_data.short_.len = (unsigned char)len;
    }
    SIJSON_CONSTEXPR20 std::size_t short_len(void) const noexcept { return this->m_data.short_.len; }

    SIJSON_CONSTEXPR20 void copy_short(const strdata_base& rhs) noexcept
    {
        this->m_data.short_ = rhs.m_data.short_;
    }

protected:
    data m_data;
};

template <typename T, typename Allocator,
    bool = strdata_base<T, Allocator>::is_long_pointer_pod::value
>
struct strdata : strdata_base<T, Allocator>
{
    using StrdataBase = strdata_base<T, Allocator>;
    using long_pointer_t = typename StrdataBase::long_pointer_t;

    SIJSON_CONSTEXPR20 strdata(void) noexcept : StrdataBase() {}
    SIJSON_CONSTEXPR20 strdata(const strdata& rhs) noexcept : StrdataBase(rhs.m_data) {}

    SIJSON_CONSTEXPR20 strdata& operator=(const strdata& rhs) noexcept { this->m_data = rhs.m_data; }

    SIJSON_CONSTEXPR20 void set_is_long(bool value) noexcept
    {
        if (value)
            this->m_data.long_.is_long = 1; // activate long
        else this->m_data.short_.is_long = 0; // activate short
    }

    SIJSON_CONSTEXPR20 void long_bufp(long_pointer_t p) noexcept { this->m_data.long_.bufp = p; }
    SIJSON_CONSTEXPR20 long_pointer_t long_bufp(void) const noexcept { return this->m_data.long_.bufp; }
};


// Fancy pointer support.
// todo: can be constexpr20! T must have trivial/constexpr dtor.
// aligned_storage_for<> cannot be constexpr since it reinterpret_casts
// see https://akrzemi1.wordpress.com/2012/12/13/constexpr-unions/
// and https://github.com/akrzemi1/Optional/blob/master/optional.hpp
//
template <typename T, typename Allocator>
struct strdata<T, Allocator, false> : strdata_base<T, Allocator>
{
    using StrdataBase = strdata_base<T, Allocator>;
    using long_pointer_t = typename StrdataBase::long_pointer_t;

    strdata(void) noexcept : StrdataBase() {}

    strdata(const strdata& rhs) noexcept : StrdataBase()
    {
        if (rhs.is_long())
        {
            this->m_data.long_.bufp.construct(rhs.m_data.long_.bufp.get());
            this->m_data.long_.cap = rhs.m_data.long_.cap;
            this->m_data.long_.len = rhs.m_data.long_.len;
        }
        else this->m_data.short_ = rhs.m_data.short_;
    }

    ~strdata(void) noexcept
    {
        if (this->is_long())
            this->m_data.long_.bufp.destroy();
    }

    inline strdata& operator=(const strdata& rhs) noexcept
    {
        if (this != &rhs)
        {
            if (rhs.is_long())
            {
                if (!this->is_long())
                    this->m_data.long_.bufp.construct();

                this->m_data.long_.is_long = rhs.m_data.long_.is_long;
                this->m_data.long_.cap = rhs.m_data.long_.cap;
                this->m_data.long_.len = rhs.m_data.long_.len;
                this->m_data.long_.bufp.get() = rhs.m_data.long_.bufp.get();
            }
            else {
                if (this->is_long())
                    this->m_data.long_.bufp.destroy();
                this->m_data.short_ = rhs.m_data.short_;
            }
        }
        return *this;
    }

    inline void set_is_long(bool value) noexcept
    {
        if (value)
        {
            if (!this->is_long())
            {
                this->m_data.long_.is_long = 1;
                this->m_data.long_.bufp.construct();
            }
        }
        else if (this->is_long())
        {
            this->m_data.long_.bufp.destroy();
            this->m_data.short_.is_long = 0;
        }
    }

    inline void long_bufp(const long_pointer_t& p) noexcept { this->m_data.long_.bufp.get() = p; }
    inline void long_bufp(long_pointer_t&& p) noexcept { this->m_data.long_.bufp.get() = std::move(p); }

    inline long_pointer_t long_bufp(void) const noexcept { return this->m_data.long_.bufp.get(); }
};

// buffer with short-string optimization.
//
// Elements are left uninitialized if alloc.construct() is trivial.
// All member functions have a strong exception guarantee if alloc.destroy() is noexcept.
//
template <typename T,
    typename Traits = std::char_traits<T>,
    typename Allocator = std::allocator<T>
>
class strbuffer : private iutil::alloc_holder<Allocator>
{
private:
    using Allocholder = iutil::alloc_holder<Allocator>;
    using Strdata = strdata<T, Allocator>;
    using Altraits = std::allocator_traits<Allocator>;

public:
    static_assert(iutil::is_pod<T>::value, "T must be a plain-old-data type.");

    using pointer = typename Altraits::pointer;
    using const_pointer = typename Altraits::const_pointer;
    using iterator = T*;
    using const_iterator = const T*;
    using allocator_type = Allocator;

public:
    // Capacity should include space for null-terminator (if any).
    strbuffer(std::size_t capacity,
        const Allocator& alloc = Allocator()
    ) :
        Allocholder(iutil::in_place, alloc)
    {
        bool sso = capacity <= Strdata::SHORT_STRING_CAP;
        this->m_str.set_is_long(!sso);

        if (sso)
            this->m_str.short_len(0);
        else
        {
            auto new_cap = iutil::align_allocn_size<T>(this->alloc(), capacity);
            this->m_str.long_bufp(iutil::alloc_new(this->alloc(), new_cap));
            this->m_str.long_cap(new_cap);
            this->m_str.long_len(0);
        }
    }

    strbuffer(const Allocator& alloc = Allocator()) :
        strbuffer(Strdata::SHORT_STRING_CAP, alloc)
    {}

    strbuffer(strbuffer&& rhs) :
        Allocholder(rhs), m_str(rhs.m_str)
    {
        if (rhs.m_str.is_long())
            rhs.m_str.long_bufp(nullptr);
    }

    strbuffer(const strbuffer& rhs) :
        Allocholder(rhs)
    {
        if (!rhs.m_str.is_long())
            this->m_str.copy_short(rhs.m_str);
        else
            copy_assign(rhs, this->alloc());
    }

    strbuffer& operator=(const strbuffer& rhs)
    {
        if (this != &rhs)
        {
            if (Altraits::propagate_on_container_copy_assignment::value)
            {
                if (iutil::alloc_is_always_equal<Allocator>::value ||
                    this->alloc() == rhs.alloc())
                {
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                    copy_assign(rhs, this->alloc());
                }
                else {
                    // else always reallocate (see Allocator requirements)
                    auto alloc = rhs.alloc();
                    copy_assign<true>(rhs, alloc);
                    iutil::copy_alloc_if_pocca(this->alloc(), rhs.alloc());
                }
            }
            else copy_assign(rhs, this->alloc());
        }
        return *this;
    }

    strbuffer& operator=(strbuffer&& rhs)
        noexcept(iutil::alloc_always_equal_after_move<Allocator>())
    {
        if (this != &rhs)
        {
            if (iutil::alloc_always_equal_after_move<Allocator>() ||
                this->alloc() == rhs.alloc())
            {
                if (this->m_str.is_long())
                    iutil::alloc_unchecked_delete(this->alloc(),
                        this->m_str.long_bufp(), this->m_str.long_len());

                // steal
                this->m_str = rhs.m_str;
                if (rhs.m_str.is_long())
                    rhs.m_str.long_bufp(nullptr);
            }
            // else copy (see Allocator requirements)
            else copy_assign(rhs, this->alloc());

            iutil::move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        }
        return *this;
    }

    inline allocator_type get_allocator(void) const { return this->alloc(); }

    inline std::size_t length(void) const noexcept
    {
        return this->m_str.is_long() ? this->m_str.long_len() : this->m_str.short_len();
    }

    inline void set_length(std::size_t new_length) noexcept
    {
        SIJSON_ASSERT(new_length <= capacity());
        this->m_str.is_long() ?
            this->m_str.long_len(new_length) :
            this->m_str.short_len(new_length);
    }

    inline std::size_t capacity(void) const noexcept
    {
        return this->m_str.is_long() ? this->m_str.long_cap() : Strdata::SHORT_STRING_CAP;
    }

    // Guaranteed to be at least 1.
    static constexpr std::size_t min_capacity(void) noexcept { return Strdata::SHORT_STRING_CAP; }

    // Capacity should include space for null-terminator (if any).
    inline void reserve(std::size_t new_cap)
    {
        if (new_cap <= capacity())
            return;

        grow(new_cap);
    }

    // Mark the next count chars as under use.
    inline void commit(std::size_t count)
    {
        SIJSON_ASSERT(length() + count <= capacity());
        this->m_str.is_long() ?
            this->m_str.long_len(this->m_str.long_len() + count) :
            this->m_str.short_len(this->m_str.short_len() + count);
    }

    inline iterator begin(void) noexcept
    {
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) : this->m_str.short_bufp();
    }

    inline iterator end(void) noexcept
    {
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) + this->m_str.long_len() :
            this->m_str.short_bufp() + this->m_str.short_len();
    }

    inline const_iterator begin(void) const noexcept
    {
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) : this->m_str.short_bufp();
    }

    inline const_iterator end(void) const noexcept
    {
        return this->m_str.is_long() ?
            iutil::to_address(this->m_str.long_bufp()) + this->m_str.long_len() :
            this->m_str.short_bufp() + this->m_str.short_len();
    }

    inline T* pbegin(void) noexcept { return iutil::to_address(begin()); }
    inline T* pend(void) noexcept { return iutil::to_address(end()); }

    inline const T* pbegin(void) const noexcept { return iutil::to_address(begin()); }
    inline const T* pend(void) const noexcept { return iutil::to_address(end()); }

    inline T& operator[](std::size_t index) noexcept { return begin()[index]; }
    inline const T& operator[](std::size_t index) const noexcept { return begin()[index]; }

    inline memspan<T> data(void) noexcept { return { pbegin(), pend() }; }
    inline memspan<const T> data(void) const noexcept { return { pbegin(), pend() }; }

    ~strbuffer(void)
    {
        if (this->m_str.is_long())
            iutil::alloc_delete(this->alloc(), this->m_str.long_bufp(), this->m_str.long_cap());
    }

private:
    template <bool IsShort>
    inline void grow_and_copy(Allocator& alloc, std::size_t rqd_cap, const T* src, std::size_t len)
    {
        SIJSON_ASSERT(rqd_cap >= len && rqd_cap > capacity());

        auto new_cap = iutil::vector_allocn_size<T>(alloc, capacity(), rqd_cap);
        pointer bufp = iutil::alloc_new(alloc, new_cap);
        iutil::scope_guard<Allocator> guard(alloc, bufp, new_cap);

        Traits::copy(iutil::to_address(bufp), src, len);

        if (!IsShort)
            iutil::alloc_unchecked_delete(this->alloc(), this->m_str.long_bufp(), this->m_str.long_cap());
        else this->m_str.set_is_long(true);

        this->m_str.long_bufp(bufp);
        this->m_str.long_cap(new_cap);
        this->m_str.long_len(len);

        guard.disable();
    }

    // Assumes this != &rhs.
    template <bool IsShort>
    inline void grow_and_copy(Allocator& alloc, const strbuffer& rhs)
    {
        grow_and_copy<IsShort>(alloc, rhs.m_str.long_len(),
            iutil::to_address(rhs.m_str.long_bufp()), rhs.m_str.long_len());
    }

    inline void grow(std::size_t rqd_cap)
    {
        if (!this->m_str.is_long())
            grow_and_copy<true>(this->alloc(), rqd_cap,
                this->m_str.short_bufp(), this->m_str.short_len());
        else
            grow_and_copy<false>(this->alloc(), rqd_cap,
                iutil::to_address(this->m_str.long_bufp()), this->m_str.long_len());
    }

    // Assumes this != &rhs.
    template <bool MustReallocate = false>
    inline void copy_assign(const strbuffer& rhs, Allocator& alloc)
    {
        if (!this->m_str.is_long())
        {
            if (!rhs.m_str.is_long()) {
                this->m_str.copy_short(rhs.m_str);
            }
            else if (rhs.m_str.long_len() <= Strdata::SHORT_STRING_CAP)
            {
                Traits::copy(this->m_str.short_bufp(),
                    iutil::to_address(rhs.m_str.long_bufp()), rhs.m_str.long_len());
                this->m_str.short_len(rhs.m_str.long_len());
            }
            else grow_and_copy<true>(alloc, rhs);
        }
        else if (!rhs.m_str.is_long())
        {
            iutil::alloc_unchecked_delete(this->alloc(), this->m_str.long_bufp(), this->m_str.long_cap());
            this->m_str = rhs.m_str;
        }
        else if (!MustReallocate && rhs.m_str.long_len() <= this->m_str.long_cap())
        {
            Traits::copy(iutil::to_address(this->m_str.long_bufp()),
                iutil::to_address(rhs.m_str.long_bufp()), rhs.m_str.long_len());
            this->m_str.long_len(rhs.m_str.long_len());
        }
        else grow_and_copy<false>(alloc, rhs);
    }

private:
    Strdata m_str;
};


// Fixed-size array stream buffer (modeled after C++23's std::basic_spanbuf).
template <typename CharT>
class basic_memspanbuf : public std::basic_streambuf<CharT, std::char_traits<CharT>>
{
public:
    using char_type = CharT;
    using traits_type = std::char_traits<CharT>;
    using int_type = typename traits_type::int_type;
    using pos_type = typename traits_type::pos_type;
    using off_type = typename traits_type::off_type;

private:
    using base = std::basic_streambuf<CharT, traits_type>;

public:
    basic_memspanbuf(CharT* src, std::streamsize size,
        std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) :
        base(), m_mode(mode)
    {
        SIJSON_ASSERT(src);
        setbuf(src, size);
    }

    basic_memspanbuf(memspan<CharT> src,
        std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) :
        basic_memspanbuf(src.begin, std::streamsize(src.end - src.begin), mode)
    {}

    template <std::size_t N>
    basic_memspanbuf(CharT(&src)[N],
        std::ios_base::openmode mode = std::ios_base::in | std::ios_base::out) :
        basic_memspanbuf(src, (std::streamsize)N, mode)
    {
        static_assert(N <= std::numeric_limits<std::streamsize>::max(), "Array is too large.");
    }

    basic_memspanbuf(basic_memspanbuf&&) = default;
    basic_memspanbuf(const basic_memspanbuf&) = delete;

    basic_memspanbuf& operator=(basic_memspanbuf&&) = default;
    basic_memspanbuf& operator=(const basic_memspanbuf&) = delete;

    // Get a span referencing the put area if 
    // ios_base::out is set in the open mode, or a
    // span referencing the entire array otherwise.
    inline memspan<CharT> data(void) const
    {
        if (m_mode & std::ios_base::out)
            return { this->pbase(), this->pptr() };
        else
            return { this->eback(), this->egptr() };
    }

protected:
    base* setbuf(CharT* src, std::streamsize size) final
    {
        if (m_mode & std::ios_base::in)
            this->setg(src, src, src + size);

        if (m_mode & std::ios_base::out)
            this->setp(src, src + size);

        return this;
    }

private:
    std::ios_base::openmode m_mode;
};

using memspanbuf = basic_memspanbuf<char>;
}

// C locale strtod
namespace util {

#ifdef SIJSON_HAS_STRTOFP_L

#ifdef _MSC_VER
using locale_t = ::_locale_t;
#else
using ::locale_t;
#endif

#ifdef _MSC_VER
#define SIJSON_NEW_CLOC ::_create_locale(LC_ALL, "C")
#else
#define SIJSON_NEW_CLOC ::newlocale(LC_ALL_MASK, "C", 0)
#endif

#if defined(__APPLE__) || defined(__FreeBSD__)
#define SIJSON_GET_CLOC NULL

#elif defined(__NetBSD__)
#define SIJSON_GET_CLOC LC_C_LOCALE

#else

#ifdef SIJSON_NO_MAGIC_STATICS
template <typename T = void>
struct cloc_inst
{
    static std::once_flag initd;
    static iutil::locale_t loc;
    static void init() noexcept { loc = SIJSON_NEW_CLOC; }
};

template <typename T>
std::once_flag cloc_inst<T>::initd;

template <typename T>
iutil::locale_t cloc_inst<T>::loc;
#endif

SIJSON_NEVER_INLINE inline 
iutil::locale_t c_loc() noexcept
{
#ifdef SIJSON_NO_MAGIC_STATICS
    std::call_once(cloc_inst<>::initd, cloc_inst<>::init);
    return cloc_inst<>::loc;
#else
    static iutil::locale_t res = SIJSON_NEW_CLOC;
    return res;
#endif
}
#define SIJSON_GET_CLOC iutil::c_loc()
#endif

// C-locale string to fp conversion. T is float, double or long double.
template <typename T>
SIJSON_ALWAYS_INLINE T strtofp(const char* src, char** eptr) noexcept;

template <>
SIJSON_ALWAYS_INLINE float strtofp(const char* src, char** eptr) noexcept {
    return ::SIJSON_MSVC_OR_POSIX(strtof_l)(src, eptr, SIJSON_GET_CLOC);
}
template <>
SIJSON_ALWAYS_INLINE double strtofp(const char* src, char** eptr) noexcept {
    return ::SIJSON_MSVC_OR_POSIX(strtod_l)(src, eptr, SIJSON_GET_CLOC);
}
template <>
SIJSON_ALWAYS_INLINE long double strtofp(const char* src, char** eptr) noexcept {
    return ::SIJSON_MSVC_OR_POSIX(strtold_l)(src, eptr, SIJSON_GET_CLOC);
}

#endif
}}

}
#endif