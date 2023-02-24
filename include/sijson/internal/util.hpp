//
// Internal utilities.
// Should not have dependencies outside of internal/.
//

#ifndef SIJSON_INTERNAL_UTIL_HPP
#define SIJSON_INTERNAL_UTIL_HPP

#include <cstddef>
#include <cassert>
#include <cstdlib>
#include <cstdio>
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
#include <new>

#include "config.hpp"

#if SIJSON_HAS_STRING_VIEW
#include <string_view>
#endif

// abi::__cxa_demangle()
#if SIJSON_HAS_CXXABI_H
#include <cxxabi.h>
#endif


#ifndef SIJSON_STRFY
#define SIJSON_STRFY(x) #x
#define SIJSON_XSTRFY(x) SIJSON_STRFY(x)
#endif

#ifndef SIJSON_SRCLOC
#define SIJSON_SRCLOC __FILE__ ":" SIJSON_XSTRFY(__LINE__) ": "
#endif

#ifndef SIJSON_ASSERT
#if SIJSON_DEBUG

#ifdef NDEBUG
#define SIJSON_ASSERT(condition) \
(void)( \
    (!!(condition)) || \
    (::sijson::internal::util::crash( \
        SIJSON_SRCLOC __func__ ": Assertion '" #condition "' failed."), 0) \
)
#else
#define SIJSON_ASSERT(condition) (assert(condition))
#endif
#else
#define SIJSON_ASSERT(condition)
#endif
#endif


namespace sijson {
namespace internal {
namespace util {
}}}

namespace sijson {
// defined up here so iutil std replacements can be fully named to avoid conflict with std 
namespace iutil = internal::util; 
namespace internal {
namespace util {


template <bool test, typename T = int>
using enable_if_t = typename std::enable_if<test, T>::type;

template <typename A, typename B, typename T = int>
using enable_if_same_t = enable_if_t<std::is_same<A, B>::value, T>;



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

#if defined(__cpp_lib_void_t) || SIJSON_CPLUSPLUS >= 201703L
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


template <typename T, typename ...Ts>
using is_one_of = iutil::disjunction<std::is_same<T, Ts>...>;


template <typename T, bool Enable>
struct add_const_if { using type = T; };

template <typename T>
struct add_const_if<T, true> { using type = const T; };

template <typename T, bool Enable>
using add_const_if_t = typename add_const_if<T, Enable>::type;


template <std::size_t ...Vals>
struct max_size {};

template <>
struct max_size<> : std::integral_constant<size_t, 0> {};

template <size_t Val>
struct max_size<Val> : std::integral_constant<size_t, Val> {};

template <std::size_t Lhs, std::size_t Rhs, std::size_t ...Rest>
struct max_size<Lhs, Rhs, Rest...> : max_size<(Lhs > Rhs ? Lhs : Rhs), Rest...>::type
{};

template <std::size_t ...Vals>
using max_size_t = typename max_size<Vals...>::type;


template <typename T>
using to_bool_t = std::integral_constant<bool, bool(T::value)>;



static constexpr auto int32_max = 0x7FFFFFFF;
static constexpr auto int32_min = -0x7FFFFFFF - 1;
static constexpr auto uint32_max = 0xFFFFFFFF;

static constexpr auto int64_max = 0x7FFFFFFFFFFFFFFF;
static constexpr auto int64_min = -0x7FFFFFFFFFFFFFFF - 1;
static constexpr auto uint64_max = 0xFFFFFFFFFFFFFFFF;


inline void crash(const char* message)
{
    std::fputs(message, stderr);
    std::abort();
}

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


// Buffer size at least as large as required to represent 
// a numerical value as decimal text (includes sign).
template <typename T, bool = std::is_floating_point<T>::value>
struct max_chars10 : std::integral_constant<int,
    // max precision + exponent (any standard fp type can't have an
    // exponent larger than int, see min_exponent) + sign + point + e
    std::numeric_limits<T>::max_digits10 + (std::numeric_limits<int>::digits10 + 2) + 3>
{};

// Buffer size at least as large as required to represent 
// a numerical value as decimal text (includes sign).
template <typename T>
struct max_chars10<T, false> : std::integral_constant<int,
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
// Behavior is undefined if the value is not convertible.
template <typename T,
    T lbound = std::numeric_limits<T>::min(),
    T ubound = std::numeric_limits<T>::max()>
inline
#if !SIJSON_DEBUG
constexpr
#endif
T uneg(make_unsigned_t<T> uvalue) noexcept
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

#if SIJSON_DEBUG
    SIJSON_ASSERT(uvalue <= absu(lbound));
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


// -------------- Memory utils -------------- 

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
    static_assert(!std::is_function<T>::value, "T must not be a function type.");
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



// https://en.cppreference.com/w/cpp/compiler_support/17, P0137R1
// available in MSVC STL from VS 2017 15.7
// available in libc++ from 6.0
// clang, gcc have a builtin (__builtin_launder)
//
#if defined(__cpp_lib_launder) || \
    (_MSC_FULL_VER >= 191426428 && SIJSON_CPLUSPLUS >= 201703L) || \
    (_LIBCPP_VERSION >= 6000 && _LIBCPP_STD_VER > 14)

using std::launder;
#else

template <typename T>
T* launder(T* p) noexcept
{
    static_assert(!std::is_function<T>::value && !std::is_void<T>::value,
        "T cannot be a function type or cv-void.");

#if SIJSON_HAS_BUILTIN(__builtin_launder)
    return __builtin_launder(p);
#else
    // todo.
    // Assume that if we cannot find an implementation, just returning is okay.
    // This assumption holds for older compilers (2014 or earlier, except for a 
    // specific case in GCC. see https://miyuki.github.io/2016/10/21/std-launder.html or
    // https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2017/p0532r0.pdf, pg 6).
    // For newer compilers (2017 or later), an implementation might be necessary.
    return p;
#endif
}
#endif


// Destroy a non-array type.
template <typename T>
inline void destroy(T& o)
{
    static_assert(!std::is_array<T>::value, "T must not be an array type.");
    o.~T();
}


// Provides properly aligned POD storage for T.
template <typename T>
class aligned_storage_for
{
public:
    template <typename ...Args>
    inline void construct(Args&&... args)
    {
        ::new (&m_buf) T(std::forward<Args>(args)...);
    }

    inline void destroy(void) { iutil::destroy(get()); }

    inline T* ptr(void) noexcept
    {
#if SIJSON_LAUNDER_ALIGNED_STORAGE
        return iutil::launder(reinterpret_cast<T*>(&m_buf));
#else
        return reinterpret_cast<T*>(&m_buf);
#endif
    }
    inline const T* ptr(void) const noexcept
    {
#if SIJSON_LAUNDER_ALIGNED_STORAGE
        return iutil::launder(reinterpret_cast<const T*>(&m_buf));
#else
        return reinterpret_cast<const T*>(&m_buf);
#endif
    }

    inline T& get(void) noexcept { return *ptr(); }
    inline const T& get(void) const noexcept { return *ptr(); }

private:
    // gcc produces false positives with -Wcast-align=strict.
    // see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=43976,
    // this is the workaround suggested by gcc.
    //
    // todo: workaround is technically UB since P0137R1.
    // https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1413r2.pdf
    //
#ifdef __GNUC__
    struct alignas(T) st 
    { unsigned char s[sizeof(T)]; } m_buf;
#else
    alignas(T) unsigned char m_buf[sizeof(T)];
#endif
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
struct alloc_has_custom_construct<void_t<
    decltype(std::declval<Allocator&>().construct(std::declval<T*>(), std::declval<CtorArgs>()...))>,
    Allocator, T*, CtorArgs...> : std::true_type
{};

template <typename, typename, typename = void>
struct alloc_has_custom_destroy : std::false_type {};

template <typename Allocator, typename T>
struct alloc_has_custom_destroy<Allocator, T, void_t<
    decltype(std::declval<Allocator&>().destroy(std::declval<T*>()))>> : std::true_type
{};

// todo: it's possible to optimize for polymorphic_allocator and scoped_allocator_adapter too, in theory

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
    tag_move_if(lhs, std::move(rhs), iutil::to_bool_t<
        typename std::allocator_traits<T>::propagate_on_container_move_assignment>{});
}

template <typename T>
inline void copy_alloc_if_pocca(T& lhs, const T& rhs) noexcept
{
    tag_copy_if(lhs, rhs, iutil::to_bool_t<
        typename std::allocator_traits<T>::propagate_on_container_copy_assignment>{});
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

    inline T& ebco_value(void) noexcept { return *this; }
    inline const T& ebco_value(void) const noexcept { return *this; }
};

template <typename T, bool EnableEbcoFallback>
class ebco<T, EnableEbcoFallback, false>
{
public:
    template <typename ...Args>
    explicit ebco(iutil::in_place_t, Args&&... args) :
        m_value(std::forward<Args>(args)...)
    {}

    inline T& ebco_value(void) noexcept { return m_value; }
    inline const T& ebco_value(void) const noexcept { return m_value; }

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

    inline T& alloc(void) noexcept { return this->ebco_value(); }
    inline const T& alloc(void) const noexcept { return this->ebco_value(); }
};

template <typename T>
constexpr bool alloc_always_equal_after_move(void) noexcept
{
    return alloc_is_always_equal<T>::value ||
        std::allocator_traits<T>::propagate_on_container_move_assignment::value;
}


}}
}

#endif