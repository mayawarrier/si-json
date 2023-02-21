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

#if SIJSON_HAS_OPTIONAL
#include <optional>
#endif

namespace sijson {

enum token : unsigned
{
    TOKEN_begin_object,
    TOKEN_end_object,
    TOKEN_begin_array,
    TOKEN_end_array,
    TOKEN_key_separator,
    TOKEN_item_separator,
    TOKEN_number,
    TOKEN_string,
    TOKEN_boolean,
    TOKEN_null,
    TOKEN_eof // End of stream
};

enum doc_node_type : unsigned
{
    DOCNODE_array,
    DOCNODE_object,
    DOCNODE_key,
    DOCNODE_value,
    DOCNODE_root,
    // Number of docnode types.
    NUM_DOCNODE_TYPES
};

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

struct io_contiguous {};
struct io_buffered {};
struct io_basic {};
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
        // allows memspan<char> --> memspan<const char> implicitly
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



#if SIJSON_HAS_OPTIONAL
template <typename T>
using optional = std::optional<T>;

static constexpr std::nullopt_t nullopt = std::nullopt;
#else

namespace internal {

struct nullopt_t
{
    constexpr explicit nullopt_t(int) {}
};

// Supports:
// - conversion to bool, has_value()
// - -> and * operators
// - default ctor (has_value() == false)
// - nullopt_t ctor
// - forwarding ctor from value i.e. optional(U&&)
// - copy/move ctors from optional of same type
// - copy/move operator= from optional of same type
template <typename T>
class optional
{
public:
    template <typename U = T, iutil::enable_if_t<
        // prevent conflict with other ctors
        !std::is_same<optional, iutil::remove_cvref_t<U>>::value> = 0>
    optional(U&& value) :
        m_has_value(true)
    {
        m_value.construct(std::forward<U>(value));
    }

    optional(void) :
        m_has_value(false)
    {}
    optional(nullopt_t) :
        m_has_value(false)
    {}

    optional(const optional& rhs) :
        m_has_value(rhs.m_has_value)
    {
        if (rhs.m_has_value)
            m_value.construct(rhs.m_value.get());
    }

    optional(optional&& rhs) :
        m_has_value(rhs.m_has_value)
    {
        if (rhs.m_has_value)
            m_value.construct(std::move(rhs.m_value.get()));
    }

    inline optional& operator=(const optional& rhs)
    {
        if (this != &rhs)
        {
            if (rhs.m_has_value)
            {
                if (!this->m_has_value)
                {
                    m_value.construct(rhs.m_value.get());
                    this->m_has_value = true;
                }
                else m_value.get() = rhs.m_value.get();
            }
            else if (this->m_has_value)
            {
                m_value.destroy();
                this->m_has_value = false;
            }
        }
        return *this;
    }

    inline optional& operator=(optional&& rhs)
    {
        if (this != &rhs)
        {
            if (rhs.m_has_value)
            {
                if (!this->m_has_value)
                {
                    m_value.construct(std::move(rhs.m_value.get()));
                    this->m_has_value = true;
                }
                else m_value.get() = std::move(rhs.m_value.get());
            }
            else if (this->m_has_value)
            {
                m_value.destroy();
                this->m_has_value = false;
            }
        }
        return *this;
    }

    inline bool has_value(void) const noexcept { return m_has_value; }
    inline explicit operator bool() const noexcept { return m_has_value; }

    inline T* operator->() noexcept { return m_value.ptr(); }
    inline const T* operator->() const noexcept { return m_value.ptr(); }

    inline T& operator*() & noexcept { return m_value.get(); }
    inline const T& operator*() const& noexcept { return m_value.get(); }

    inline T&& operator*() && noexcept { return std::move(m_value.get()); }
    inline const T&& operator*() const&& noexcept { return std::move(m_value.get()); }

    ~optional()
    {
        if (m_has_value)
            m_value.destroy();
    }
private:
    iutil::aligned_storage_for<T> m_value;
    bool m_has_value;
};
}

template <typename T>
using optional = internal::optional<T>;

static constexpr internal::nullopt_t nullopt{ 0 };
#endif


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

// safer to use T and CharT to guarantee sfinae
// https://cplusplus.github.io/CWG/issues/1558.html
template <typename T, typename CharT>
using check_io_typedefs = iutil::void_t<
    iutil::enable_if_same_t<typename T::char_type, CharT>,
    iutil::enable_if_t<iutil::is_nb_unsigned_integral<typename T::size_type>::value>>;


template <typename, typename, typename = void>
struct is_input : std::false_type {};

template <typename T, typename CharT>
struct is_input<T, CharT, iutil::void_t<
    check_io_typedefs<T, CharT>, typename T::input_kind,
    iutil::enable_if_same_t<decltype(std::declval<T>().peek()), CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().take()), CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().ipos()), typename T::size_type>,
    iutil::enable_if_same_t<decltype(std::declval<T>().end()), bool>,
    decltype(std::declval<T>().rewind())>> :
std::true_type
{};


template <typename, typename, typename = void>
struct is_output : std::false_type {};

template <typename T, typename CharT>
struct is_output<T, CharT, iutil::void_t<
    check_io_typedefs<T, CharT>, typename T::output_kind,
    decltype(std::declval<T>().put(std::declval<CharT>())),
    decltype(std::declval<T>().put_f(std::declval<CharT>(), std::declval<typename T::size_type>())),
    decltype(std::declval<T>().put_n(std::declval<const CharT*>(), std::declval<std::size_t>())),
    iutil::enable_if_same_t<decltype(std::declval<T>().opos()), typename T::size_type>,
    decltype(std::declval<T>().flush())>> :
std::true_type
{};


template <typename, typename, typename = void>
struct is_contiguous_input : std::false_type {};

template <typename T, typename CharT>
struct is_contiguous_input<T, CharT, iutil::void_t<
    iutil::enable_if_t<is_input<T, CharT>::value>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().ipbeg()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().ipend()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().ipcur()), const CharT*>,
    decltype(std::declval<T>().icommit(std::declval<typename T::size_type>()))>> :
    std::true_type
{};


template <typename, typename, typename = void>
struct is_contiguous_output : std::false_type {};

template <typename T, typename CharT>
struct is_contiguous_output<T, CharT, iutil::void_t<
    iutil::enable_if_t<is_output<T, CharT>::value>,
    decltype(std::declval<T>().reserve(std::declval<typename T::size_type>())),
    iutil::enable_if_same_t<decltype(std::declval<T>().opbeg()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<T>().opend()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<T>().opcur()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().opbeg()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().opend()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().opcur()), const CharT*>,
    decltype(std::declval<T>().ocommit(std::declval<typename T::size_type>()))>> :
std::true_type
{};

}
}

#endif