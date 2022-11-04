
#ifndef SIJSON_STREAM_WRAPPERS_HPP
#define SIJSON_STREAM_WRAPPERS_HPP

#include <cstddef>
#include <type_traits>
#include <limits>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <stdexcept>

#include "internal/util.hpp"


namespace sijson {

// Adapts a std::basic_istream to sijson's istream.
// 
// Note: this class respects the user's exception mask, 
// which can produce undefined behavior in cases other
// than those allowed by sijson's istream concept.
// 
template <typename StdIstream = std::istream>
class std_istream_wrapper
{
private:
    using Traits = typename StdIstream::traits_type;

public:
    using char_type = typename StdIstream::char_type;
    using streamsize_type = iutil::make_unsigned_t<std::streamoff>;
    using underlying_stream_type = StdIstream;

public:
    std_istream_wrapper(StdIstream& stream) :
        m_stream(stream)
    {}

    std_istream_wrapper(std_istream_wrapper&&) = default;
    std_istream_wrapper(const std_istream_wrapper&) = default;

    // reference member deletes both assignment operators
    std_istream_wrapper& operator=(std_istream_wrapper&&) = delete;
    std_istream_wrapper& operator=(const std_istream_wrapper&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char_type peek(void) { return Traits::to_char_type(m_stream.peek()); }

    // Extract character. If end(), behavior is undefined.
    inline char_type take(void) { return Traits::to_char_type(m_stream.get()); }

    // Get input position.
    inline streamsize_type inpos(void) 
    {
        // fpos<> is only convertible to std::streamoff
        return (std::streamoff)m_stream.tellg(); 
    }

    // True if the last operation completed by reaching the end of the stream.
    inline bool end(void) const { return m_stream.eof(); }

    // Jump to the beginning of the stream.
    inline void rewind(void) { m_stream.seekg(0); }

private:
    StdIstream& m_stream;
};



// Adapts a std::basic_ostream to this library's ostream.
template <typename StdOstream = std::ostream>
class std_ostream_wrapper
{
public:
    using char_type = typename StdOstream::char_type;
    using traits_type = typename StdOstream::traits_type;
    using underlying_stream_type = StdOstream;
    
public:
    std_ostream_wrapper(StdOstream& stream) :
        m_stream(stream)
    {}

    std_ostream_wrapper(std_ostream_wrapper&&) = default;
    std_ostream_wrapper(const std_ostream_wrapper&) = default;

    // reference member deletes both assignment operators
    std_ostream_wrapper& operator=(std_ostream_wrapper&&) = delete;
    std_ostream_wrapper& operator=(const std_ostream_wrapper&) = delete;

    // Put a character.
    inline void put(char_type c) { m_stream.put(c); }

    // Put the same character multiple times.
    inline void put(char_type c, std::size_t count)
    {
        std::fill_n(std::ostreambuf_iterator<char_type, traits_type>(m_stream), count, c);
    }

    // Put characters from an array.
    inline void put_n(const char_type* str, std::size_t count)
    {
#if SIJSON_PREFER_LOGIC_ERRORS
        if (count > std::numeric_limits<std::streamsize>::max())
            throw std::length_error("Size larger than std::streamsize.");
#else
        assert(count <= std::numeric_limits<std::streamsize>::max());
#endif
        m_stream.write(str, (std::streamsize)count);
    }

    // Synchronize with target.
    inline void flush(void) { m_stream.flush(); }

    // Get output position.
    inline std::size_t outpos(void) { return m_stream.tellp(); }

private:
    StdOstream& m_stream;
};


template <typename T>
struct istream_traits
{
    // True if T inherits from std::basic_istream.
    static constexpr bool is_wrapped = iutil::inherits_std_basic_istream<T>::value;

    using type = T;
    // std_istream_wrapper<T> if T inherits from std::basic_istream, else U.
    template <typename U>
    using wrapper_type_or = typename std::conditional<
        iutil::inherits_std_basic_istream<T>::value, std_istream_wrapper<T>, U>::type;
};

template <typename T>
struct ostream_traits
{
    // True if T inherits from std::basic_ostream.
    static constexpr bool is_wrapped = iutil::inherits_std_basic_ostream<T>::value;

    using type = T;
    // std_ostream_wrapper<T> if T inherits from std::basic_ostream, else U.
    template <typename U>
    using wrapper_type_or = typename std::conditional<
        iutil::inherits_std_basic_ostream<T>::value, std_ostream_wrapper<T>, U>::type;
};


template <typename T>
using wrap_if_not_sijson_istream_t = typename istream_traits<T>::template wrapper_type_or<T&>;

template <typename T>
using wrap_if_not_sijson_ostream_t = typename ostream_traits<T>::template wrapper_type_or<T&>;

}
#endif
