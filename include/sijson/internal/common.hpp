// 
// Definitions shared by internal/ and outside.
// 

#ifndef SIJSON_INTERNAL_COMMON_HPP
#define SIJSON_INTERNAL_COMMON_HPP

#include <cstddef>
#include <cassert>
#include <type_traits>

#include "util.hpp"

namespace sijson {

enum token_t : unsigned
{
    TOKEN_eof, // End of stream
    TOKEN_begin_object,
    TOKEN_end_object,
    TOKEN_begin_array,
    TOKEN_end_array,
    TOKEN_key_separator,
    TOKEN_item_separator,
    TOKEN_number,
    TOKEN_string,
    TOKEN_boolean,
    TOKEN_null
};

enum doc_node_t : unsigned
{
    DOCNODE_array,
    DOCNODE_object,
    DOCNODE_key,
    DOCNODE_value,
    DOCNODE_root,
    // Number of docnode types.
    NUM_DOCNODE_TYPES
};

inline const char* doc_node_name(doc_node_t type)
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

template <bool value>
using null_terminated_t = std::integral_constant<bool, value>;

// Tag type to indicate stream should throw if an
// I/O operation will exhaust the stream before the
// operation is complete.
template <bool value>
using throw_on_overflow_t = std::integral_constant<bool, value>;


// Describes a continguous section of memory.
template <typename T>
struct memspan
{
    constexpr memspan(T* begin, T* end) noexcept :
        begin(begin), end(end)
    {}

    template <typename U,
        // Check if U* is implicitly convertible to T*
        // via cv-qualification conversion only
        // https://stackoverflow.com/questions/42992663/
        iutil::require_t<std::is_convertible<U(*)[], T(*)[]>::value> = 0
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
}

#endif