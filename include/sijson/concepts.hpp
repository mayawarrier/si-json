//
// Defines the core concepts in SIJSON and type traits to detect them.
//

#ifndef SIJSON_CONCEPTS
#define SIJSON_CONCEPTS

#include <cstddef>
#include <type_traits>
#include <utility>

#include "internal/common.hpp"
#include "internal/util.hpp"

namespace json {

template <typename, typename = void>
struct is_istream : std::false_type {};

//
// is_istream<T>::value is true if T is an input stream i.e. it implements:
// - char peek(); 
// --- Get character. If end(), behavior is undefined.
// - char take(); 
// --- Extract character. If end(), behavior is undefined.
// - std::size_t inpos(); 
// --- Get input position (num characters taken).
// - bool end(); 
// --- True if the last operation reached the end of
// --- the stream (i.e. no more chars can be extracted).
//
template <typename T>
struct is_istream<T, iutil::void_t<
    iutil::require_same_t<decltype(std::declval<T>().peek()), char>,
    iutil::require_same_t<decltype(std::declval<T>().take()), char>,
    iutil::require_same_t<decltype(std::declval<T>().inpos()), std::size_t>,
    iutil::require_same_t<decltype(std::declval<T>().end()), bool>>> : std::true_type
{};

template <typename, typename = void>
struct is_ostream : std::false_type {};

//
// is_ostream<T>::value is true if T is an output stream i.e. it implements:
// - put(char c); 
// --- Put a character.
// - put(char c, std::size_t count); 
// --- Put the same character count times.
// - putn(const char* str, std::size_t count); 
// --- Put count characters from an array.
// - flush(); 
// --- Synchronize with target.
// - std::size_t outpos();
// --- Get output position (num characters put).
//
template <typename T>
struct is_ostream<T, iutil::void_t<
    decltype(std::declval<T>().put(std::declval<char>())),
    decltype(std::declval<T>().put(std::declval<char>(), std::declval<std::size_t>())),
    decltype(std::declval<T>().putn(std::declval<const char*>(), std::declval<std::size_t>())),
    decltype(std::declval<T>().flush()),
    iutil::require_same_t<decltype(std::declval<T>().outpos()), std::size_t>>> : std::true_type
{};

template <typename, typename = void>
struct is_idata_spannable : std::false_type {};

//
// is_idata_spannable<T>::value is true if T implements:
// - std::size_t inlength() const;
// --- The total number of elements available for input.
// --- For example, if T is an input stream, inlength() should return the
// --- number of chars that can be extracted before end() is true.
// - memspan<const char> indata() const;
// --- A span of the entire input data (from position 0 to inlength()).
//
template <typename T>
struct is_idata_spannable<T, iutil::void_t<
    iutil::require_same_t<decltype(std::declval<const T>().inlength()), std::size_t>,
    iutil::require_same_t<decltype(std::declval<const T>().indata()), memspan<const char>>>> : std::true_type
{};

template <typename, typename = void>
struct is_odata_spannable : std::false_type {};

//
// is_odata_spannable<T>::value is true if T implements:
// - memspan<const char> outdata() const;
// --- A span of all the data sent to output. Does not require the data to be
// --- flushed. For example, for an output stream, this should return a span
// --- from position 0 to outpos().
//
template <typename T>
struct is_odata_spannable<T, iutil::void_t<
    iutil::require_same_t<decltype(std::declval<const T>().outdata()), memspan<const char>>>> : std::true_type
{};

}

#endif