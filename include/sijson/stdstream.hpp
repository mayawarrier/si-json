
#ifndef SIJSON_STDSTREAM
#define SIJSON_STDSTREAM

#include <cstddef>
#include <type_traits>
#include <limits>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <stdexcept>

#include "internal/util.hpp"


namespace sijson {

// Adapts a std::basic_istream to this library's istream.
template <typename BasicIstream = std::istream>
class std_istream_wrapper
{
public:
    static_assert(std::is_same<char,
        typename BasicIstream::char_type>::value,
        "Stream char_type must be char.");

    using traits_type = typename BasicIstream::traits_type;
    using wrapped_type = BasicIstream;

public:
    std_istream_wrapper(BasicIstream& stream) :
        m_stream(stream)
    {}

    std_istream_wrapper(std_istream_wrapper&&) = default;
    std_istream_wrapper(const std_istream_wrapper&) = default;

    // reference member deletes both assignment operators
    std_istream_wrapper& operator=(std_istream_wrapper&&) = delete;
    std_istream_wrapper& operator=(const std_istream_wrapper&) = delete;

    // Get character. If end(), behavior is undefined.
    inline char peek(void) { return traits_type::to_char_type(m_stream.peek()); }

    // Extract character. If end(), behavior is undefined.
    inline char take(void) { return traits_type::to_char_type(m_stream.get()); }

    // Get input position.
    inline std::size_t inpos(void) { return m_stream.tellg(); }

    // True if the last operation completed by reaching the end of the stream.
    inline bool end(void) const { return m_stream.eof(); }

    // Jump to the beginning of the stream.
    inline void rewind(void) { m_stream.seekg(0, std::ios_base::beg); }

private:
    BasicIstream& m_stream;
};



// Adapts a std::basic_ostream to this library's ostream.
template <typename BasicOstream = std::ostream>
class std_ostream_wrapper
{
public:
    static_assert(std::is_same<char,
        typename BasicOstream::char_type>::value,
        "Stream char_type must be char.");

    using traits_type = typename BasicOstream::traits_type;
    using wrapped_type = BasicOstream;
    
public:
    std_ostream_wrapper(BasicOstream& stream) :
        m_stream(stream)
    {}

    std_ostream_wrapper(std_ostream_wrapper&&) = default;
    std_ostream_wrapper(const std_ostream_wrapper&) = default;

    // reference member deletes both assignment operators
    std_ostream_wrapper& operator=(std_ostream_wrapper&&) = delete;
    std_ostream_wrapper& operator=(const std_ostream_wrapper&) = delete;

    // Put a character.
    inline void put(char c) { m_stream.put(c); }

    // Put the same character multiple times.
    inline void put(char c, std::size_t count)
    {
        std::fill_n(std::ostreambuf_iterator<char, traits_type>(m_stream), count, c);
    }

    // Put characters from an array.
    inline void putn(const char* str, std::size_t count)
    {
        if (count > std::numeric_limits<std::streamsize>::max())
            throw std::length_error("Size larger than std::streamsize.");

        m_stream.write(str, (std::streamsize)count);
    }

    // Synchronize with target.
    inline void flush(void) { m_stream.flush(); }

    // Get output position.
    inline std::size_t outpos(void) { return m_stream.tellp(); }

private:
    BasicOstream& m_stream;
};


// Wraps T with a std_istream_wrapper if necessary.
// If T inherits from std::basic_istream, this is
// std_istream_wrapper<remove_cvref_t<T>>, else T.
template <typename T>
using wrap_std_istream_t = typename std::conditional<
    iutil::inherits_std_basic_istream<iutil::remove_cvref_t<T>>::value,
    std_istream_wrapper<iutil::remove_cvref_t<T>>, T>::type;

// Wraps T with a std_ostream_wrapper if necessary.
// If T inherits from std::basic_ostream, this is
// std_ostream_wrapper<remove_cvref_t<T>>, else T.
template <typename T>
using wrap_std_ostream_t = typename std::conditional<
    iutil::inherits_std_basic_ostream<iutil::remove_cvref_t<T>>::value,
    std_ostream_wrapper<iutil::remove_cvref_t<T>>, T>::type;

}
#endif
