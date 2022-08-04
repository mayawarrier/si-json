
#ifndef SIJSON_NUMBER_HPP
#define SIJSON_NUMBER_HPP

#include <cstdint>
#include <cassert>
#include <stdexcept>

#include "internal/util.hpp"

namespace sijson {

// Arbitrary numerical value (integral or floating-point).
class number final
{
public:
    enum type_t : int
    {
        TYPE_float,
        TYPE_double,
        TYPE_intmax_t,
        TYPE_uintmax_t
    };

public:
    template <typename T,
        iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value, int> = 0>
    number(T value) noexcept : m_intg(value), m_type(TYPE_intmax_t)
    {}

    template <typename T, 
        iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value, int> = 0>
    number(T value) noexcept : m_uintg(value), m_type(TYPE_uintmax_t)
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
    // Throws if value held is not of type T.
    template <typename T>
    inline T get(void) const;

    // Get value.
    // Behavior is undefined if value held is not of type T.
    template <typename T>
    inline T get_nothrow(void) const noexcept;

    // Get value, cast to type T.
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
};
template <> struct number::typehelper<double> 
{
    static constexpr int typeidx = TYPE_double;
    static inline double get(const number& n) noexcept { return n.m_dbl; }
};
template <> struct number::typehelper<std::intmax_t>
{
    static constexpr int typeidx = TYPE_intmax_t;
    static inline std::intmax_t get(const number& n) noexcept { return n.m_intg; }
};
template <> struct number::typehelper<std::uintmax_t>
{
    static constexpr int typeidx = TYPE_uintmax_t;
    static inline std::uintmax_t get(const number& n) noexcept { return n.m_uintg; }
};

// Get value.
// Behavior is undefined if value held is not of type T.
template <typename T>
inline T number::get_nothrow(void) const noexcept
{
    return typehelper<T>::get(*this);
}

// Get value.
// Throws if value held is not of type T.
template <typename T>
inline T number::get(void) const
{
    if (typehelper<T>::typeidx != m_type)
        throw std::logic_error("Value held is not of type T");

    return get_nothrow<T>();
}

// Get value, cast to type T.
template <typename T>
inline T number::as(void) const noexcept
{
    switch (m_type)
    {
        case TYPE_float: return (T)m_flt;
        case TYPE_double: return (T)m_dbl;
        case TYPE_intmax_t: return (T)m_intg;
        case TYPE_uintmax_t: return (T)m_uintg;
        default: assert(false); return {};
    }
}

}
#endif