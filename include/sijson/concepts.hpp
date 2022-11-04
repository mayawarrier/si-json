//
// Defines the core concepts in SIJSON and type traits to detect them.
//

#ifndef SIJSON_CONCEPTS
#define SIJSON_CONCEPTS

#include <cstddef>
#include <ios>
#include <limits>
#include <type_traits>
#include <utility>

#include "core.hpp"
#include "internal/util.hpp"

namespace sijson {

namespace internal
{
// safer to use T and CharT to guarantee sfinae
// https://cplusplus.github.io/CWG/issues/1558.html
template <typename T, typename CharT>
using check_stream_has_valid_member_types = iutil::void_t<
    iutil::enable_if_same_t<typename T::char_type, CharT>,
    iutil::enable_if_t<iutil::is_nb_unsigned_integral<typename T::streamsize_type>::value>>;
}

template <typename, typename, typename = void>
struct is_istream : std::false_type {};
//
// is_istream<T, CharT>::value is true if T implements:
// - char_type peek(); 
// -- Get character. If end(), behavior is undefined.
// - char_type take(); 
// -- Extract character. If end(), behavior is undefined.
// - streamsize_type inpos(); 
// -- Get input position.
// - bool end(); 
// -- True if the last operation reached the end of the stream.
// - rewind();
// -- Jump to the beginning of the stream.
// T must also define the member types:
// - char_type = CharT;
// - streamsize_type
// -- implementation-defined unsigned integral type large
// -- enough to hold the maximum size of the stream
//
template <typename T, typename CharT>
struct is_istream<T, CharT, iutil::void_t<
    internal::check_stream_has_valid_member_types<T, CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().peek()), CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().take()), CharT>,
    iutil::enable_if_same_t<decltype(std::declval<T>().inpos()), typename T::streamsize_type>,
    iutil::enable_if_same_t<decltype(std::declval<T>().end()), bool>,
    decltype(std::declval<T>().rewind())>> : 
    std::true_type
{};


template <typename, typename, typename = void>
struct is_ostream : std::false_type {};
//
// is_ostream<T, CharT>::value is true if T implements:
// - put(char_type c); 
// -- Put a character.
// - put(char_type c, std::size_t count); 
// -- Put the same character count times.
// - put_n(const char_type* s, std::size_t count); 
// -- Put count characters from the array pointed to by s.
// -streamsize_type outpos();
// -- Get output position.
// - flush(); 
// -- Synchronize with target device.

// T must also define the member types:
// - char_type = CharT;
// - streamsize_type
// -- implementation-defined unsigned integral type large
// -- enough to hold the maximum size of the stream
//
template <typename T, typename CharT>
struct is_ostream<T, CharT, iutil::void_t<
    internal::check_stream_has_valid_member_types<T, CharT>,
    decltype(std::declval<T>().put(std::declval<CharT>())),
    decltype(std::declval<T>().put(std::declval<CharT>(), std::declval<std::size_t>())),
    decltype(std::declval<T>().put_n(std::declval<const CharT*>(), std::declval<std::size_t>())),
    iutil::enable_if_same_t<decltype(std::declval<T>().outpos()), typename T::streamsize_type>,
    decltype(std::declval<T>().flush())>> :
    std::true_type
{};


template <typename, typename, typename = void>
struct is_contiguous_istream : std::false_type {};
//
// is_contiguous_istream<T, CharT>::value is true if T satisfies 
// is_istream<T, CharT> and further implements:
// - const char_type* inpbegin() const;
// -- Pointer to the first char in the stream.
// - const char_type* inpend() const;
// -- Pointer to one past the last char in the stream.
// - const char_type* inpcur() const;
// -- Pointer to the next char returned by peek() or take().
// - incommit(streamsize_type count);
// -- Mark the next count characters in the stream as read.
// -- If count is larger than the size of the remaining
// -- data in the stream, the behavior is undefined.
// -- After this call, inpos() increases by count.
//
// The following conditions are also expected to hold:
// - inpbegin() <= inpcur() <= inpend()
// - inpbegin() and inpend() remain unchanged after calls
// - to peek(), take(), inpos(), end(), rewind(), or any 
// - const member functions.
//
template <typename T, typename CharT>
struct is_contiguous_istream<T, CharT, iutil::void_t<
    iutil::enable_if_t<is_istream<T, CharT>::value>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().inpbegin()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().inpend()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().inpcur()), const CharT*>,
    decltype(std::declval<T>().incommit(std::declval<typename T::streamsize_type>()))>> :
    std::true_type
{};


template <typename, typename, typename = void>
struct is_contiguous_ostream : std::false_type {};
//
// is_contiguous_ostream<T, CharT>::value is true if T satisfies
// is_ostream<T, CharT> and also implements:
// - char_type* outpbegin(); const char_type* outpbegin() const;
// -- Pointer to the first char in the stream. 
// - char_type* outpend(); const char_type* outpend() const;
// -- Pointer to one past the last char of reserved/writable memory.
// - char_type* outpcur(); const char_type* outpcur() const;
// -- Pointer to the next char modified by put().
// - outcommit(streamsize_type count);
// -- Mark the next count characters in the stream as initialized
// -- (i.e. the same as having been written by put() or put_n()).
// -- A call to this must only be required if stream data in the range 
// -- [outpcur(), outpend()) has been modified through pointers derived from 
// -- outpbegin(), outpcur(), or outpend(). If count is larger than the 
// -- remaining reserved memory, the behavior is undefined. After this call,
// -- outpos() increases by count.
//
// The following conditions are also expected to hold:
// - outpbegin() <= outpcur() <= outpend()
// - outpbegin() and outpend() remain unchanged after calls to outpos()
// - or any const member functions.
//
template <typename T, typename CharT>
struct is_contiguous_ostream<T, CharT, iutil::void_t<
    iutil::enable_if_t<is_ostream<T, CharT>::value>,
    iutil::enable_if_same_t<decltype(std::declval<T>().outpbegin()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<T>().outpend()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<T>().outpcur()), CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().outpbegin()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().outpend()), const CharT*>,
    iutil::enable_if_same_t<decltype(std::declval<const T>().outpcur()), const CharT*>,
    decltype(std::declval<T>().outcommit(std::declval<typename T::streamsize_type>()))>> :
    std::true_type
{};

}

#endif