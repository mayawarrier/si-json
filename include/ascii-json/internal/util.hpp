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
#include <type_traits>
#include <memory>
#include <string>
#include <iostream>
#include <utility>
#include <algorithm>
#include <stdexcept>


#ifndef SIJSON_CPLUSPLUS
#ifdef _MSC_VER
#define SIJSON_CPLUSPLUS _MSVC_LANG
#else
#define SIJSON_CPLUSPLUS __cplusplus
#endif
#endif

#ifndef SIJSON_HAS_STRING_VIEW
#if defined(__cpp_lib_string_view) || SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STRING_VIEW
#endif
#endif

#ifdef SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif


namespace json {
namespace internal {
namespace util {

template <bool test, typename T = int>
using require_t = typename std::enable_if<test, T>::type;

template <typename A, typename B, typename T = int>
using require_same_t = require_t<std::is_same<A, B>::value, T>;


// Arrays decay to const T*, all other Ts stay the same.
// See https://stackoverflow.com/questions/12374746/.
template <typename T>
using decay_array_to_constptr_t = typename std::decay<const T&>::type;

// True if T is an instance of the template Templ.
template <typename T, template <typename...> class Templ>
struct is_instance_of : std::false_type {};

template <typename ...Templ_args, template <typename...> class Templ>
struct is_instance_of<Templ<Templ_args...>, Templ> : std::true_type {};


#if defined (__cpp_lib_logical_traits) || SIJSON_CPLUSPLUS >= 201703L
template <typename ...Bn>
using conjunction = std::conjunction<Bn...>;
#else
// https://en.cppreference.com/w/cpp/types/conjunction
template <typename...> struct conjunction : std::true_type { };
template <typename B1> struct conjunction<B1> : B1 { };
template <typename B1, typename ...Bn>
struct conjunction<B1, Bn...> : std::conditional<bool(B1::value), conjunction<Bn...>, B1>::type {};
#endif


#if defined(__cpp_lib_void_t) || SIJSON_CPLUSPLUS >= 201703L
template <typename ...Ts>
using void_t = std::void_t<Ts...>;
#else
// https://en.cppreference.com/w/cpp/types/void_t
// https://cplusplus.github.io/CWG/issues/1558.html
template <typename ...Ts> struct make_void { typedef void type; };
template <typename ...Ts> using void_t = typename make_void<Ts...>::type;
#endif


#if defined(__cpp_lib_remove_cvref) || SIJSON_CPLUSPLUS >= 202002L
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

#ifdef SIJSON_HAS_STRING_VIEW
// True if T is basic_string_view<CharT, ...>.
template <typename T, typename CharT>
using is_instance_of_basic_string_view = std::integral_constant<bool, conjunction<
    is_instance_of<T, std::basic_string_view>, has_value_type<T, CharT>>::value>;
#endif

// True if T is a string type supported by this library for writing.
template <typename T, typename CharT>
using is_writable_string_type = std::integral_constant<bool,
    std::is_same<typename std::decay<T>::type, CharT*>::value ||
    std::is_same<typename std::decay<T>::type, const CharT*>::value ||
    is_instance_of_basic_string<T, CharT>::value
#ifdef SIJSON_HAS_STRING_VIEW
    || is_instance_of_basic_string_view<T, CharT>::value
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


// Max # of chars required to represent an integral value as decimal text (includes sign).
template <typename T>
struct integral_max_chars10 : std::integral_constant<int,
    // round up digits10 + sign
    std::numeric_limits<T>::digits10 + 1 + std::is_signed<T>::value>
{};

// Max # of chars required to represent a numerical value as decimal text (includes sign).
template <typename T>
struct max_chars10 : std::conditional<
    std::is_floating_point<T>::value,
    // max precision + exponent + sign + point + e/E
    std::integral_constant<int,
        std::numeric_limits<T>::max_digits10 + integral_max_chars10<int>::value + 3>,
    integral_max_chars10<T>>::type
{};


template <typename T, typename = void>
struct alloc_is_always_equal : std::is_empty<T> {};

// added in a C++11 defect report, check if present
template <typename T>
struct alloc_is_always_equal<T, void_t<
    typename std::allocator_traits<T>::is_always_equal>> :
    std::allocator_traits<T>::is_always_equal
{};


template <typename, typename...> // ok! less appropriate than partial specialization
struct alloc_has_custom_construct : std::false_type {};

template <typename Allocator, typename Ptr, typename ...CtorArgs>
struct alloc_has_custom_construct<void_t<
    decltype(std::declval<Allocator&>().construct(std::declval<Ptr>(), std::declval<CtorArgs>()...))>, 
    Allocator, Ptr, CtorArgs...> : std::true_type
{};

template <typename, typename, typename = void>
struct alloc_has_custom_destroy : std::false_type {};

template <typename Allocator, typename Ptr>
struct alloc_has_custom_destroy<Allocator, Ptr, void_t<
    decltype(std::declval<Allocator&>().destroy(std::declval<Ptr>()))>> : std::true_type
{};


// True if allocator.construct() must be called for a given allocator, type and ctor arguments.
template <typename Allocator, typename T, typename ...CtorArgs>
using alloc_must_construct = std::integral_constant<bool, !std::is_trivially_constructible<T>::value || 
    (!is_instance_of<Allocator, std::allocator>::value && alloc_has_custom_construct<Allocator, T*, CtorArgs...>::value)>;

// True if allocator.destroy() must be called for a given allocator and type.
template <typename Allocator, typename T>
using alloc_must_destroy = std::integral_constant<bool, !std::is_trivially_destructible<T>::value ||
    (!is_instance_of<Allocator, std::allocator>::value && alloc_has_custom_destroy<Allocator, T*>::value)>;

// Rebind an allocator to a different value_type.
template <typename Allocator, typename T>
using rebind_alloc_t = typename std::allocator_traits<Allocator>::template rebind_alloc<T>;


// Equivalent to new for custom allocators.
// Strong exception guarantee if alloc.destroy(...) is noexcept.
template <typename T, typename Allocator>
inline T* alloc_new(Allocator& alloc, std::size_t size)
{
    using Altraits = std::allocator_traits<Allocator>;

    T* ptr = Altraits::allocate(alloc, size);

    // value-initialize only if absolutely necessary
    if (alloc_must_construct<Allocator, T>::value)
    {
        std::size_t i;
        try {
            for (i = 0; i < size; ++i)
                Altraits::construct(alloc, ptr + i);
        }
        catch (...) {
            if (alloc_must_destroy<Allocator, T>::value)
                for (std::size_t j = 0; j < i; ++j)
                    Altraits::destroy(alloc, ptr + j);

            Altraits::deallocate(alloc, ptr, size);
            throw;
        }
    }
    return ptr;
}

// alloc_delete() but does not check for null.
// Noexcept if alloc.destroy(...) is noexcept.
template <typename T, typename Allocator>
inline void alloc_unchecked_delete(Allocator& alloc, T* ptr, std::size_t size)
{
    using Altraits = std::allocator_traits<Allocator>;

    if (alloc_must_destroy<Allocator, T>::value)
        for (std::size_t i = 0; i < size; ++i)
            Altraits::destroy(alloc, ptr + i);

    Altraits::deallocate(alloc, ptr, size);
}

// Equivalent to delete for custom allocators.
// Noexcept if alloc.destroy(...) is noexcept.
template <typename T, typename Allocator>
inline void alloc_delete(Allocator& alloc, T* ptr, std::size_t size)
{
    if (ptr) alloc_unchecked_delete(alloc, ptr, size);
}


// Recommend a better allocation size.
template <typename T, typename Allocator>
inline std::size_t recommend_alloc_size(Allocator& alloc, std::size_t size)
{
    auto max_size = std::allocator_traits<Allocator>::max_size(alloc);

    constexpr std::size_t alignment = 16; // usually
    constexpr std::size_t value_alignment =
        sizeof(T) < alignment ? alignment / sizeof(T) : 1;

    if (size > max_size - value_alignment)
        return max_size;
    auto aligned = (size + value_alignment) & ~value_alignment;

    if (aligned > max_size)
        return max_size;

    return aligned;
}

// Recommend a better allocation size for a vector-like container.
template <typename T, typename Allocator>
inline std::size_t recommend_vector_growth(Allocator& alloc, std::size_t old_size, std::size_t new_size)
{
    auto max_size = std::allocator_traits<Allocator>::max_size(alloc);
    return old_size <= max_size / 2 ?
        recommend_alloc_size<T>(alloc, std::max(new_size, 2 * old_size)) : max_size;
}

template <typename T, typename Allocator>
class exception_guard
{
public:
    exception_guard(Allocator& alloc, T* data, std::size_t size) noexcept :
        m_alloc(alloc), m_data(data), m_size(size), m_enabled(true)
    {}

    inline void disable(void) noexcept { m_enabled = false; }

    ~exception_guard()
    {
        if (m_enabled)
            alloc_delete(m_alloc, m_data, m_size);
    }
private:
    Allocator& m_alloc;
    T* m_data;
    std::size_t m_size;
    bool m_enabled;
};


template <typename T>
inline void move_alloc_if(T& lhs, T&& rhs, std::true_type) noexcept
{ lhs = std::move(rhs); }

template <typename T>
inline void move_alloc_if(T&, T&&, std::false_type) noexcept {}

template <typename T>
inline void copy_alloc_if(T& lhs, const T& rhs, std::true_type) noexcept
{ lhs = rhs; }

template <typename T>
inline void copy_alloc_if(T&, const T&, std::false_type) noexcept {}


template <typename T>
inline void move_alloc_if_pocma(T& lhs, T&& rhs) noexcept
{
    move_alloc_if(lhs, std::move(rhs),
        std::integral_constant<bool, // may be derived, convert
            std::allocator_traits<T>::propagate_on_container_move_assignment::value>{});
}

template <typename T>
inline void copy_alloc_if_pocca(T& lhs, const T& rhs) noexcept
{
    copy_alloc_if(lhs, rhs,
        std::integral_constant<bool, // may be derived, convert
            std::allocator_traits<T>::propagate_on_container_copy_assignment::value>{});
}


template <typename T,
#if defined(__cpp_lib_is_final) || SIJSON_CPLUSPLUS >= 201402L
    bool = std::is_empty<T>::value && !std::is_final<T>::value>
#else
    bool = is_instance_of<T, std::allocator>::value>
#endif
class alloc_ebo : private T
{
public:
    alloc_ebo(void) 
        noexcept(noexcept(T())) : T()
    {}

    template <typename U, 
        typename = require_same_t<T, remove_cvref_t<U>>>
    alloc_ebo(U&& alloc) noexcept :
        T(std::forward<U>(alloc))
    {}

    inline T& alloc(void) noexcept { return *this; }
    inline const T& alloc(void) const noexcept { return *this; }
};

template <typename T>
class alloc_ebo<T, false>
{
public:
    alloc_ebo(void)
        noexcept(noexcept(T())) : m_alloc()
    {}

    template <typename U,
        typename = require_same_t<T, remove_cvref_t<U>>>
    alloc_ebo(U&& alloc) noexcept : 
        m_alloc(std::forward<U>(alloc))
    {}

    inline T& alloc(void) noexcept { return m_alloc; }
    inline const T& alloc(void) const noexcept { return m_alloc; }

private:
    T m_alloc;
};

// Wraps an allocator that implements std::allocator_traits.
// Tries to apply EBO on the allocator type.
// See https://en.cppreference.com/w/cpp/language/ebo.
template <typename T>
class alloc_aware_container : protected alloc_ebo<T>
{
private:
    using base = alloc_ebo<T>;

public:
    alloc_aware_container(void) 
        noexcept(noexcept(base())) : base()
    {}

    template <typename U, 
        typename = require_same_t<T, remove_cvref_t<U>>>
    alloc_aware_container(U&& alloc) noexcept :
        base(std::forward<U>(alloc))
    {}

    alloc_aware_container(alloc_aware_container&& rhs) noexcept = default;

    alloc_aware_container(const alloc_aware_container& rhs) noexcept :
        base(std::allocator_traits<T>::select_on_container_copy_construction(rhs.alloc()))
    {}

    inline alloc_aware_container& operator=(alloc_aware_container&& rhs) noexcept
    {
        if (this != &rhs)
            move_alloc_if_pocma(this->alloc(), std::move(rhs.alloc()));
        return *this;
    }
    inline alloc_aware_container& operator=(const alloc_aware_container& rhs) noexcept
    {
        if (this != &rhs)
            copy_alloc_if_pocca(this->alloc(), rhs.alloc());
        return *this;
    }
};

template <typename T>
constexpr bool alloc_always_equal_after_move(void) noexcept
{
    return alloc_is_always_equal<T>::value ||
        std::allocator_traits<T>::propagate_on_container_move_assignment::value;
}





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