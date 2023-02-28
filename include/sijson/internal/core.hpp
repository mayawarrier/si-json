// 
// Common definitions (shared by internal/ and outside).
// 

#ifndef SIJSON_INTERNAL_CORE_HPP
#define SIJSON_INTERNAL_CORE_HPP

#include <cstddef>
#include <cstdint>
#include <ios>
#include <utility>
#include <string>
#include <type_traits>
#include <stdexcept>

#include "config.hpp"
#include "util.hpp"


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

enum doc_node_type : unsigned
{
    DOCNODE_array,
    DOCNODE_object,
    DOCNODE_key,
    DOCNODE_value,
    DOCNODE_root
};
// Number of docnode types.
static constexpr int NUM_DOCNODE_TYPES = 5;

inline const char* docnode_name(doc_node_type type)
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

enum error : int
{
    ERROR_str_escape = -3,
    ERROR_str_delim = -2,
    ERROR_bad_rdflags = -1,
    ERROR_none = 0
};

inline const char* get_error_msg(error e)
{
    switch (e)
    {
    case ERROR_str_escape: 
        return "Invalid string escape.";
    case ERROR_str_delim: 
        return "String delimiter missing.";
    case ERROR_bad_rdflags:
        return "Invalid read flag(s).";
    case ERROR_none: 
        return "No error.";
    default: 
        return "Unknown error.";
    }
}

template <typename OffT>
inline std::runtime_error parse_error(OffT offset, const char* msg)
{
    return std::runtime_error("JSON parse error at offset " + std::to_string(offset) + ": " + msg);
}
template <typename OffT>
inline std::runtime_error parse_error(OffT offset, const std::string& msg)
{
    return parse_error(offset, msg.c_str());
}
template <typename OffT>
inline std::runtime_error parse_error(OffT offset, error err)
{
    return parse_error(offset, get_error_msg(err));
}


namespace tag
{
// Tag type: string stream should be null-terminated.
struct null_terminated {};

// Tag type: throw if an I/O operation will result in buffer overflow.
struct throw_on_overflow {};


struct io_basic 
{
    static constexpr unsigned flags = 0x1;
};
struct io_buffered 
{
    static constexpr unsigned flags = 0x3;
};
struct io_contiguous 
{
    static constexpr unsigned flags = 0x5;
};
}


// Describes a contiguous section of memory.
template <typename T>
struct memspan
{
    constexpr memspan(T* begin, T* end) noexcept :
        begin(begin), end(end)
    {}

    template <typename U,
        // Check if U* differs from T* only by cv-qualification, this
        // allows eg. memspan<char> --> memspan<const char> implicitly
        // https://stackoverflow.com/questions/42992663/
        iutil::enable_if_t<std::is_convertible<U(*)[], T(*)[]>::value> = 0
    >
    constexpr memspan(const memspan<U>& rhs) noexcept :
        begin(rhs.begin), end(rhs.end)
    {}

    inline std::size_t size(void) const noexcept
    {
        return (std::size_t)(end - begin);
    }

    T* begin;
    // Always one past the last element.
    T* end;
};


// Span of the stream's remaining input data i.e. { in_curp(), in_endp() }.
template <typename ContiguousIstream>
inline memspan<const typename ContiguousIstream::char_type> indata(const ContiguousIstream& is)
{
    return { is.ipcur(), is.ipend() };
}

// Span of the stream's entire input data i.e. { in_begp(), in_endp() }.
template <typename ContiguousIstream>
inline memspan<const typename ContiguousIstream::char_type> inarea(const ContiguousIstream& is)
{
    return { is.ipbeg(), is.ipend() };
}

// Num input chars remaining.
template <typename ContiguousInput>
inline typename ContiguousInput::size_type inrem(const ContiguousInput& is)
{
    return is.ipend() - is.ipcur();
}

// Span of the stream's output data { opbeg(), opcur() }.
// Span may be invalidated if a non-const reference to
// the stream is passed to a function or if any non-const
// member functions are called on the stream.
template <typename ContiguousOstream>
inline memspan<iutil::add_const_if_t<typename ContiguousOstream::char_type, std::is_const<ContiguousOstream>::value>>
outdata(ContiguousOstream& os)
{
    return { os.opbeg(), os.opcur() };
}

// Span of the stream's output area { opbeg(), opend() }.
// Span may be invalidated if a non-const reference to
// the stream is passed to a function or if any non-const
// member functions are called on the stream.
template <typename ContiguousOstream>
inline memspan<iutil::add_const_if_t<typename ContiguousOstream::char_type, std::is_const<ContiguousOstream>::value>>
outarea(ContiguousOstream& os)
{
    return { os.opbeg(), os.opend() };
}



// Holds an arbitrary numerical value.
class number final
{
public:
    using largest_fp_type = double;

    enum etype : int
    {
        TYPE_float,
        TYPE_double,
        TYPE_intmax,
        TYPE_uintmax
    };

public:
    template <typename T,
        iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value> = 0>
    number(T value) noexcept : 
        m_intg(value), m_type(TYPE_intmax)
    {}

    template <typename T,
        iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value> = 0>
    number(T value) noexcept : 
        m_uintg(value), m_type(TYPE_uintmax)
    {}

    number(float value) noexcept : 
        m_flt(value), m_type(TYPE_float) 
    {}
    number(double value) noexcept : 
        m_dbl(value), m_type(TYPE_double) 
    {}

    number(void) noexcept : 
        m_dbl(0), m_type(TYPE_double) // largest range
    {} 

    number(number&&) noexcept = default;
    number(const number&) noexcept = default;

    number& operator=(number&&) noexcept = default;
    number& operator=(const number&) noexcept = default;

    // Get type.
    inline etype type(void) const noexcept { return m_type; }

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
    inline T as(void) const noexcept
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

private:
    union
    {
        float m_flt;
        double m_dbl;
        std::intmax_t m_intg;
        std::uintmax_t m_uintg;
    };
    etype m_type;

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

}

namespace sijson {
namespace internal {

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
    iutil::enable_if_t<(T::input_kind::flags & 0x4u) != 0>,
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
    iutil::enable_if_t<(T::output_kind::flags & 0x4u) != 0>,
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
struct is_nothrow_input<T, CharT, iutil::void_t<
    iutil::enable_if_t<std::is_same<typename T::input_kind, tag::io_basic>::value>>> : 
    is_nothrow_basic_input<T, CharT>
{};

template <typename T, typename CharT>
struct is_nothrow_input<T, CharT, iutil::void_t<
    iutil::enable_if_t<std::is_same<typename T::input_kind, tag::io_contiguous>::value>>> :
    is_nothrow_contiguous_input<T, CharT>
{};

template <typename, typename, typename = void>
struct is_nothrow_output : std::false_type {};

template <typename T, typename CharT>
struct is_nothrow_output<T, CharT, iutil::void_t<
    iutil::enable_if_t<std::is_same<typename T::output_kind, tag::io_basic>::value>>> :
    is_nothrow_basic_output<T, CharT>
{};

template <typename T, typename CharT>
struct is_nothrow_output<T, CharT, iutil::void_t<
    iutil::enable_if_t<std::is_same<typename T::output_kind, tag::io_contiguous>::value>>> :
    is_nothrow_contiguous_output<T, CharT>
{};

}}

#endif