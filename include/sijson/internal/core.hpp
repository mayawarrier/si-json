// 
// Definitions shared by internal/ and outside.
// 

#ifndef SIJSON_INTERNAL_COMMON_HPP
#define SIJSON_INTERNAL_COMMON_HPP

#include <cstddef>
#include <cassert>
#include <ios>
#include <utility>
#include <type_traits>

#include "config.hpp"
#include "util.hpp"
#include "util_memory.hpp"

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

inline const char* doc_node_name(doc_node_type type)
{
    switch (type)
    {
        case DOCNODE_array: return "array";
        case DOCNODE_object: return "object";
        case DOCNODE_key: return "key";
        case DOCNODE_value: return "value";
        case DOCNODE_root: return "root";
        default: assert(false); return nullptr;
    }
}


// Tag type: string stream should be null-terminated.
struct null_terminated_t
{
    constexpr explicit null_terminated_t() = default;
};
static constexpr null_terminated_t null_terminated{};


// Tag type: stream should throw if an I/O operation will not succeed
// as it will exhaust the stream before the operation is complete.
struct throw_on_overflow_t
{
    constexpr explicit throw_on_overflow_t() = default;
};
static constexpr throw_on_overflow_t throw_on_overflow{};


#if SIJSON_HAS_STDBYTE
using byte = std::byte;
#else
using byte = unsigned char;
#endif


// Describes a continguous section of memory.
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


// Span of the stream's remaining input data i.e. { inpcur(), inpend() }.
template <typename ContiguousIstream>
inline memspan<const typename ContiguousIstream::char_type> indata(const ContiguousIstream& is)
{
    return { is.inpcur(), is.inpend() };
}
// Span of the stream's entire input data i.e. { inpbegin(), inpend() }.
template <typename ContiguousIstream>
inline memspan<const typename ContiguousIstream::char_type> inarea(const ContiguousIstream& is)
{
    return { is.inpbegin(), is.inpend() };
}

// Span of the data written to the stream i.e. { outpbegin(), outpcur() }.
// Span may be invalidated if a non-const reference to
// the stream is passed to a function or if any non-const
// member functions are called on the stream.
template <typename ContiguousOstream>
inline memspan<iutil::add_const_if_t<typename ContiguousOstream::char_type, std::is_const<ContiguousOstream>::value>>
outdata(ContiguousOstream& os)
{
    return { os.outpbegin(), os.outpcur() };
}
// Span of the memory reserved by the stream i.e. { outpbegin(), outpend() }.
// Span may be invalidated if a non-const reference to
// the stream is passed to a function or if any non-const
// member functions are called on the stream.
template <typename ContiguousOstream>
inline memspan<iutil::add_const_if_t<typename ContiguousOstream::char_type, std::is_const<ContiguousOstream>::value>>
outarea(ContiguousOstream& os)
{
    return { os.outpbegin(), os.outpend() };
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

}

#endif