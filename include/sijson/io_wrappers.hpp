
#ifndef SIJSON_IO_WRAPPERS_HPP
#define SIJSON_IO_WRAPPERS_HPP

#include <cstddef>
#include <type_traits>
#include <limits>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <stdexcept>

#include "internal/core.hpp"


namespace sijson {

// Wraps a std::basic_istream.
// Respects the user's exception mask.
// 
template <typename StdIstream = std::istream>
class std_istream_wrapper
{
private:
    using Traits = typename StdIstream::traits_type;

public:
    using char_type = typename StdIstream::char_type;
    using size_type = iutil::make_unsigned_t<std::streamoff>;
    using input_kind = io_basic;
    using underlying_type = StdIstream;

public:
    std_istream_wrapper(StdIstream& is) :
        m_is(is)
    {}

    std_istream_wrapper(std_istream_wrapper&&) = default;
    std_istream_wrapper(const std_istream_wrapper&) = default;

    std_istream_wrapper& operator=(std_istream_wrapper&&) = delete;
    std_istream_wrapper& operator=(const std_istream_wrapper&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char_type peek(void) { return Traits::to_char_type(m_is.peek()); }

    // Extract character. If end(), behavior is undefined.
    inline char_type take(void) { return Traits::to_char_type(m_is.get()); }

    // Get input position.
    inline size_type ipos(void) 
    {
        // fpos<> is only convertible to std::streamoff
        return (std::streamoff)m_is.tellg(); 
    }

    // True if input has run out of characters.
    inline bool end(void) const { return m_is.eof(); }

    // Jump to the beginning of the input.
    inline void rewind(void) { m_is.seekg(0); }

private:
    StdIstream& m_is;
};



// Wraps a std::basic_ostream.
// Respects the user's exception mask.
//
template <typename StdOstream = std::ostream>
class std_ostream_wrapper
{
private:
    using Traits = typename StdOstream::traits_type;

public:
    using char_type = typename StdOstream::char_type;
    using size_type = iutil::make_unsigned_t<std::streamoff>;  
    using output_kind = io_basic;
    using underlying_type = StdOstream;
    
public:
    std_ostream_wrapper(StdOstream& os) :
        m_os(os)
    {}

    std_ostream_wrapper(std_ostream_wrapper&&) = default;
    std_ostream_wrapper(const std_ostream_wrapper&) = default;

    std_ostream_wrapper& operator=(std_ostream_wrapper&&) = delete;
    std_ostream_wrapper& operator=(const std_ostream_wrapper&) = delete;

    // Put a character.
    inline void put(char_type c) { m_os.put(c); }

    // Put a character multiple times.
    inline void put_f(char_type c, size_type count)
    {
        std::fill_n(std::ostreambuf_iterator<char_type, Traits>(m_os), count, c);
    }

    // Put characters from an array.
    inline void put_n(const char_type* str, std::size_t count)
    {
#if SIJSON_USE_LOGIC_ERRORS
        if (count > std::numeric_limits<std::streamsize>::max())
            throw std::length_error("Array too large.");
#else
        assert(count <= std::numeric_limits<std::streamsize>::max());
#endif
        // utos() avoids undefined behavior if count is too large.
        m_os.write(str, static_cast<std::streamsize>(iutil::utos(count)));
    }

    // Get output position.
    inline size_type opos(void) 
    {
        // fpos<> is only convertible to std::streamoff
        return (std::streamoff)m_os.tellp(); 
    }

    // Synchronize with target.
    inline void flush(void) { m_os.flush(); }

private:
    StdOstream& m_os;
};


template <typename T>
struct input_traits
{
    static constexpr bool requires_wrapper = iutil::inherits_std_basic_istream<T>::value;

    using type = T;
    // Wrapper type if T requires wrapper, else U.
    template <typename U>
    using wrapper_type_or = typename std::conditional<
        iutil::inherits_std_basic_istream<T>::value, std_istream_wrapper<T>, U>::type;
};

template <typename T>
struct output_traits
{
    static constexpr bool requires_wrapper = iutil::inherits_std_basic_ostream<T>::value;

    using type = T;
    // Wrapper type if T requires wrapper, else U.
    template <typename U>
    using wrapper_type_or = typename std::conditional<
        iutil::inherits_std_basic_ostream<T>::value, std_ostream_wrapper<T>, U>::type;
};

template <typename T>
using to_si_input_t = typename input_traits<T>::template wrapper_type_or<T>;

template <typename T>
using to_si_output_t = typename output_traits<T>::template wrapper_type_or<T>;


template <typename T>
using wrap_input_t = typename input_traits<T>::template wrapper_type_or<T&>;

template <typename T>
using wrap_output_t = typename output_traits<T>::template wrapper_type_or<T&>;

}
#endif
