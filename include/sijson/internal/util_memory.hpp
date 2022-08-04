
#ifndef SIJSON_INTERNAL_UTIL_MEMORY_HPP
#define SIJSON_INTERNAL_UTIL_MEMORY_HPP

#include <type_traits>
#include <memory>
#include <utility>
#include <new>

#include "config.hpp"
#include "util.hpp"


namespace sijson {
namespace internal {
namespace util {

#ifdef __cpp_lib_to_address

using std::to_address;
#else

// https://en.cppreference.com/w/cpp/memory/to_address, modified for C++11

template <typename Ptr, typename = void>
struct ptr_traits_has_to_address : std::false_type {};

template <typename Ptr>
struct ptr_traits_has_to_address<Ptr,
    void_t<decltype(std::pointer_traits<Ptr>::to_address(std::declval<Ptr>()))>> :
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
inline auto to_address(const Ptr& p) noexcept -> decltype(to_address(p.operator->()))
{
    return to_address(p.operator->());
}
#endif


// https://en.cppreference.com/w/cpp/compiler_support/17, P0137R1
// available in MSVC STL from VS 2017 15.7
// available in libc++ from 6.0
// clang, gcc have a builtin (__builtin_launder)
//
#if defined(__cpp_lib_launder) || \
    _MSC_FULL_VER >= 191426428 || \
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


// Provides POD storage for an object.
// Allows non-POD types to be placed in a union
// so common subsequence rule may apply.
template <typename T>
class pod_storage
{
public:
    template <typename ...Args>
    inline void construct(Args&&... args)
    {
        ::new (&m_buf) T(std::forward<Args>(args)...);
    }
    inline void destroy(void)
    {
        iutil::destroy(get());
    }

    inline T& get(void) noexcept
    {
#if SIJSON_USE_LAUNDER_TO_ACCESS_ALIGNED_BYTE_STORAGE
        return *iutil::launder(reinterpret_cast<T*>(&m_buf));
#else
        return *reinterpret_cast<T*>(&m_buf);
#endif
    }
    inline const T& get(void) const noexcept
    {
#if SIJSON_USE_LAUNDER_TO_ACCESS_ALIGNED_BYTE_STORAGE
        return *iutil::launder(reinterpret_cast<const T*>(&m_buf));
#else
        return *reinterpret_cast<const T*>(&m_buf);
#endif
    }
private:
    alignas(alignof(T)) unsigned char m_buf[sizeof(T)];
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


// True if T must be default-constructed for a given allocator.
template <typename Allocator, typename T = typename Allocator::value_type>
using alloc_must_default_construct = std::integral_constant<bool, !std::is_trivially_constructible<T>::value ||
    // todo: it's possible to optimize for polymorphic_allocator and scoped_allocator_adapter too, in theory
    conjunction<negation<is_instance_of<Allocator, std::allocator>>, alloc_has_custom_construct<Allocator, T>>::value>;

// True if T must be destructed for a given allocator.
template <typename Allocator, typename T = typename Allocator::value_type>
using alloc_must_destroy = std::integral_constant<bool, !std::is_trivially_destructible<T>::value ||
    // todo: it's possible to optimize for polymorphic_allocator and scoped_allocator_adapter too, in theory
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
                Altraits::construct(alloc, to_address(p) + i);
        } 
        catch (...) {
            if (alloc_must_destroy<Allocator>::value)
                for (std::size_t j = 0; j < i; ++j)
                    Altraits::destroy(alloc, to_address(p) + j);

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
            Altraits::destroy(alloc, to_address(p) + i);

    Altraits::deallocate(alloc, p, size);
}

// Equivalent to delete for custom allocators.
// Noexcept if alloc.destroy() is noexcept.
template <typename Allocator>
inline void alloc_delete(Allocator& alloc, alloc_ptr_t<Allocator> ptr, std::size_t size)
{
    if (ptr) alloc_unchecked_delete(alloc, ptr, size);
}


// Recommend a better allocation size.
template <typename T, typename Allocator>
inline std::size_t recommend_allocn_size(Allocator& alloc, std::size_t size)
{
    auto max_size = std::allocator_traits<Allocator>::max_size(alloc);

    constexpr std::size_t alignment = 16; // usually?
    constexpr std::size_t value_alignment =
        sizeof(T) < alignment ? alignment / sizeof(T) : 1;

    if (size > max_size - value_alignment + 1)
        return max_size;
    auto aligned = (size + value_alignment - 1) & ~(value_alignment - 1);

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
        recommend_allocn_size<T>(alloc, std::max(new_size, 2 * old_size)) : max_size;
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
    move_if_tag(lhs, std::move(rhs),
        std::integral_constant<bool, // may be derived, convert
        std::allocator_traits<T>::propagate_on_container_move_assignment::value>{});
}

template <typename T>
inline void copy_alloc_if_pocca(T& lhs, const T& rhs) noexcept
{
    copy_if_tag(lhs, rhs,
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
        typename = enable_if_same_t<T, remove_cvref_t<U>>>
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
        typename = enable_if_same_t<T, remove_cvref_t<U>>>
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
class alloc_holder : protected alloc_ebo<T>
{
private:
    using base = alloc_ebo<T>;

public:
    alloc_holder(void)
        noexcept(noexcept(base())) : base()
    {}

    template <typename U,
        typename = enable_if_same_t<T, remove_cvref_t<U>>>
        alloc_holder(U&& alloc) noexcept :
        base(std::forward<U>(alloc))
    {}

    alloc_holder(alloc_holder&& rhs) noexcept = default;

    alloc_holder(const alloc_holder& rhs) noexcept :
        base(std::allocator_traits<T>::select_on_container_copy_construction(rhs.alloc()))
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
};

template <typename T>
constexpr bool alloc_always_equal_after_move(void) noexcept
{
    return alloc_is_always_equal<T>::value ||
        std::allocator_traits<T>::propagate_on_container_move_assignment::value;
}

}}}

#endif