//
// Defines the concepts in SIJSON and type traits to detect them.
//

#ifndef SIJSON_CONCEPTS_HPP
#define SIJSON_CONCEPTS_HPP

#include "internal/core.hpp"

namespace sijson {

//
// true if T implements:
//
// - char_type peek(); 
//   Get character. If end(), behavior is undefined.
// - char_type take();
//   Extract character. If end(), behavior is undefined.
// - size_type ipos(); 
//   Get input position.
// - bool end(); 
//   True if input has run out of characters.
// - rewind();
//   Jump to the beginning of the input.
//
// And has typedefs:
// - char_type = CharT
// - size_type: unsigned integral type large enough
//   to hold the maximum input size
// - input_kind: an I/O tag type that is at least io_basic
//
template <typename T, typename CharT>
using is_basic_input = iconcepts::is_basic_input<T, CharT>;

//
// Same as is_basic_input<T, CharT>, but all functions are noexcept.
//
template <typename T, typename CharT>
using is_nothrow_basic_input = iconcepts::is_nothrow_basic_input<T, CharT>;


//
// true if T implements:
//
// - put(char_type c); 
//   Put a character.
// - put_f(char_type c, size_type count); 
//   Put a character count times.
// - put_n(const char_type* s, size_t count); 
//   Put count characters from the array s.
// - size_type opos();
//   Get output position.
// - flush(); 
//   Synchronize with target device.
//
// And has typedefs:
// - char_type = CharT;
// - size_type: unsigned integral type large enough
//   to hold the maximum size of the stream
// - output_kind: an I/O tag type that is at least io_basic
//
template <typename T, typename CharT>
using is_basic_output = iconcepts::is_basic_output<T, CharT>;

//
// Same as is_basic_output<T, CharT>, but all functions are noexcept.
//
template <typename T, typename CharT>
using is_nothrow_basic_output = iconcepts::is_nothrow_basic_output<T, CharT>;


//
// true if T satisfies is_basic_input<T, CharT> and implements:
// 
// - const char_type* ipbeg() const;
//   Pointer to the first char in the stream.
//
// - const char_type* ipend() const;
//   Pointer to one past the last char in the stream.
// 
// - const char_type* ipcur() const;
//   Pointer to the next char returned by peek() or take().
// 
// - icommit(size_type count);
//   Mark the next count characters in the stream as read.
//   If count is greater than the number of chars remaining
//   in the stream, the behavior is undefined. After this 
//   call, ipos() increases by count.
// 
// And has typedefs:
// - output_kind: an I/O tag type that is at least io_contiguous
//
// The following conditions must also hold:
// - ipbeg() <= ipcur() <= ipend()
// - ipbeg() and ipend() remain unchanged after calls
//   to peek(), take(), ipos(), end(), rewind(), or any 
//   const member functions.
//
template <typename T, typename CharT, typename = void>
using is_contiguous_input = iconcepts::is_contiguous_input<T, CharT>;

//
// Same as is_contiguous_input<T, CharT>, but all functions are noexcept.
//
template <typename T, typename CharT, typename = void>
using is_nothrow_contiguous_input = iconcepts::is_nothrow_contiguous_input<T, CharT>;


//
// true if T satisfies is_basic_output<T, CharT> and implements:
// 
// - char_type* opbeg(); 
//   const char_type* opbeg() const;
//   Pointer to the first char in the stream. 
// 
// - char_type* opend(); 
//   const char_type* opend() const;
//   Pointer to one past the last char of writable memory.
// 
// - char_type* opcur(); 
//   const char_type* opcur() const;
//   Pointer to the next char modified by put().
// 
// - ocommit(size_type count);
//   Mark the next count characters in the stream as written.
//   A call to this should only be required if stream data in the range 
//   [opcur(), opend()) has been modified through pointers derived from 
//   opbeg(), opcur(), or opend(). If count is greater than the 
//   remaining writable memory, the behavior is undefined. After this call,
//   outpos() increases by count.
// 
// And has typedefs:
// - output_kind: an I/O tag type that is at least io_contiguous
//
// The following conditions must also hold:
// - opbeg() <= opcur() <= opend()
// - opbeg() and opend() remain unchanged after calls to opos()
//   or any const member functions.
//
template <typename T, typename CharT, typename = void>
using is_contiguous_output = iconcepts::is_contiguous_output<T, CharT>;

//
// Same as is_contiguous_output<T, CharT>, but all functions are noexcept.
//
template <typename T, typename CharT>
using is_nothrow_contiguous_output = iconcepts::is_nothrow_contiguous_output<T, CharT>;


//
// Checks if all input functions are noexcept (for T's input_kind).
//
template <typename T, typename CharT>
using is_nothrow_input = iconcepts::is_nothrow_input<T, CharT>;

//
// Checks if all output functions are noexcept (for T's output_kind).
//
template <typename T, typename CharT>
using is_nothrow_output = iconcepts::is_nothrow_output<T, CharT>;

}

#endif