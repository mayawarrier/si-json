
#ifndef SIJSON_STREAM_ADAPTER_HPP
#define SIJSON_STREAM_ADAPTER_HPP

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
// The adapter does not throw when ios failure bits 
// are set, which may lead to undefined behavior unless the 
// 
template <typename StdIstream = std::istream>
class std_istream_adapter
{
private:
    using Traits = typename StdIstream::traits_type;

public:
    using char_type = typename StdIstream::char_type;
    using streamsize_type = iutil::make_unsigned_t<std::streamoff>;
    using wrapped_type = StdIstream;

public:
    std_istream_adapter(StdIstream& stream) :
        m_stream(stream)
    {}

    std_istream_adapter(std_istream_adapter&&) = default;
    std_istream_adapter(const std_istream_adapter&) = default;

    // reference member deletes both assignment operators
    std_istream_adapter& operator=(std_istream_adapter&&) = delete;
    std_istream_adapter& operator=(const std_istream_adapter&) = delete;

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
class std_ostream_adapter
{
public:
    using char_type = typename StdOstream::char_type;
    using traits_type = typename StdOstream::traits_type;
    using wrapped_type = StdOstream;
    
public:
    std_ostream_adapter(StdOstream& stream) :
        m_stream(stream)
    {}

    std_ostream_adapter(std_ostream_adapter&&) = default;
    std_ostream_adapter(const std_ostream_adapter&) = default;

    // reference member deletes both assignment operators
    std_ostream_adapter& operator=(std_ostream_adapter&&) = delete;
    std_ostream_adapter& operator=(const std_ostream_adapter&) = delete;

    // Put a character.
    inline void put(char_type c) { m_stream.put(c); }

    // Put the same character multiple times.
    inline void put(char_type c, std::size_t count)
    {
        std::fill_n(std::ostreambuf_iterator<char_type, traits_type>(m_stream), count, c);
    }

    // Put characters from an array.
    inline void putn(const char_type* str, std::size_t count)
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
    StdOstream& m_stream;
};


// Adapts an istream to sijson's istream.
// If T inherits from std::basic_istream, this is
// std_istream_adapter<remove_reference_t<T>>, else T.
template <typename T>
using sijson_istream_t = typename std::conditional<
    iutil::inherits_std_basic_istream<iutil::remove_reference_t<T>>::value,
    std_istream_adapter<iutil::remove_reference_t<T>>, T>::type;

// Adapts an ostream to sijson's ostream.
// If T inherits from std::basic_ostream, this is
// std_ostream_adapter<remove_reference_t<T>>, else T.
template <typename T>
using sijson_ostream_t = typename std::conditional<
    iutil::inherits_std_basic_ostream<iutil::remove_reference_t<T>>::value,
    std_ostream_adapter<iutil::remove_reference_t<T>>, T>::type;
}
#endif
