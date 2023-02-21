
#ifndef SIJSON_NUMBER_HPP
#define SIJSON_NUMBER_HPP

#include <cstdint>
#include <utility>
#include <string>
#include <stdexcept>

#include "internal/util.hpp"

namespace sijson {

// Arbitrary numerical value (integral or floating-point).
class number final
{
public:
    using largest_fp_type = double;

    enum type_t : int
    {
        TYPE_float,
        TYPE_double,
        TYPE_intmax,
        TYPE_uintmax
    };

public:
    template <typename T,
        iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value, int> = 0>
    number(T value) noexcept : m_intg(value), m_type(TYPE_intmax)
    {}

    template <typename T, 
        iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value, int> = 0>
    number(T value) noexcept : m_uintg(value), m_type(TYPE_uintmax)
    {}

    number(float value) noexcept : m_flt(value), m_type(TYPE_float) {}
    number(double value) noexcept : m_dbl(value), m_type(TYPE_double) {}

    number(void) noexcept : m_dbl(0), m_type(TYPE_double) {} // largest range

    number(number&&) noexcept = default;
    number(const number&) noexcept = default;
    
    number& operator=(number&&) noexcept = default;
    number& operator=(const number&) noexcept = default;   

    // Get type.
    inline type_t type(void) const noexcept { return m_type; }

    // Get value.
    // Throws if active type is not T.
    template <typename T>
    inline T get(void) const;

    // Get value without checking active type.
    // If not T, calling this is undefined behavior.
    template <typename T>
    inline T get_unsafe(void) const noexcept;

    // Gets value if active type is T, else returns nullptr.
    template <typename T>
    inline const T* get_if(void) const noexcept;

    // Get active value, cast to T.
    template <typename T>
    inline T as(void) const noexcept;

private:
    union 
    {
        float m_flt;
        double m_dbl;
        std::intmax_t m_intg;
        std::uintmax_t m_uintg;
    };
    type_t m_type;  

    template <typename T>
    struct typehelper 
    {
        static constexpr int typeidx = -1;
    };
};

template <> struct number::typehelper<float> 
{
    static constexpr int typeidx = TYPE_float;

    static inline float get(const number& n) noexcept { return n.m_flt; }
    static inline const float* cptr(const number& n) noexcept { return &n.m_flt; }
};
template <> struct number::typehelper<double> 
{
    static constexpr int typeidx = TYPE_double;

    static inline double get(const number& n) noexcept { return n.m_dbl; }
    static inline const double* cptr(const number& n) noexcept { return &n.m_dbl; }
};
template <> struct number::typehelper<std::intmax_t>
{
    static constexpr int typeidx = TYPE_intmax;

    static inline std::intmax_t get(const number& n) noexcept { return n.m_intg; }
    static inline const std::intmax_t* cptr(const number& n) noexcept { return &n.m_intg; }
};
template <> struct number::typehelper<std::uintmax_t>
{
    static constexpr int typeidx = TYPE_uintmax;

    static inline std::uintmax_t get(const number& n) noexcept { return n.m_uintg; }
    static inline const std::uintmax_t* cptr(const number& n) noexcept { return &n.m_uintg; }
};

// Gets value if active type is T, else returns nullptr.
template <typename T>
inline const T* number::get_if(void) const noexcept
{
    return typehelper<T>::typeidx == m_type ?
        typehelper<T>::cptr() : nullptr;
}

// Get value without checking active type.
// If not T, calling this is undefined behavior.
template <typename T>
inline T number::get_unsafe(void) const noexcept
{
    return typehelper<T>::get(*this);
}

// Get value.
// Throws if active type is not T.
template <typename T>
inline T number::get(void) const
{
    if (typehelper<T>::typeidx != m_type)
        throw std::logic_error(std::string(__func__) + ": Active type is not T.");

    return typehelper<T>::get(*this);
}

// Get active value, cast to T.
template <typename T>
inline T number::as(void) const noexcept
{
    switch (m_type)
    {
        case TYPE_float: return (T)m_flt;
        case TYPE_double: return (T)m_dbl;
        case TYPE_intmax: return (T)m_intg;
        case TYPE_uintmax: return (T)m_uintg;
        default:
#ifdef __cpp_lib_unreachable
            std::unreachable();
#else
            SIJSON_ASSERT(false);
            return {};
#endif
    }
}

}
#endif