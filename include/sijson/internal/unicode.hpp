//
// https://en.wikipedia.org/wiki/UTF-8#Codepage_layout
// https://stackoverflow.com/questions/6240055/
//

#ifndef SIJSON_INTERNAL_UNICODE_HPP
#define SIJSON_INTERNAL_UNICODE_HPP

#include <cstdint>
#include <utility>

#include "core.hpp"

namespace sijson {
namespace internal {
namespace util {

static constexpr char16_t UTF16_ERR = 0xFFFF;


template <typename CharT>
inline char16_t utf_unesc_unit(const CharT str[4], tag::io_contiguous)
{
    char16_t res = 0;
    for (int i = 0; i < 4; ++i)
    {
        auto c = str[i];
        res = (res << 4) + (char16_t)c;

        if (c >= 0x30 && c <= 0x39) // '0' to '9'
            res -= 0x30;
        else if (c >= 0x41 && c <= 0x46) // 'a' to 'f'
            res -= 0x41 - 10;
        else if (c >= 0x61 && c <= 0x66) // 'A' to 'F'
            res -= 0x61 - 10;
        else
            return UTF16_ERR;
    }
    return res;
}

template <typename SiInput>
inline char16_t utf_unesc_unit(SiInput& is, tag::io_basic)
{
    char16_t res = 0;
    for (int i = 0; i < 4; ++i)
    {
        if (is.end())
            return UTF16_ERR;

        auto c = is.peek();
        res = (res << 4) + (char16_t)c;

        if (c >= 0x30 && c <= 0x39) // '0' to '9'
            res -= 0x30;
        else if (c >= 0x41 && c <= 0x46) // 'a' to 'f'
            res -= 0x41 - 10;
        else if (c >= 0x61 && c <= 0x66) // 'A' to 'F'
            res -= 0x61 - 10;
        else
            return UTF16_ERR;

        is.take();
    }
    return res;
}

template <typename TargetCharT>
struct utf16_convert {};

template <>
struct utf16_convert<char>
{
    using cp_t = std::uint_least32_t;

    template <typename SiOutput>
    static inline void do_put(SiOutput& os, cp_t cp)
    {
        using uchar = unsigned char;
     
        if (cp < 0x80u) {
            os.put((uchar)cp); 
        }
        else if (cp < 0x800u)
        {
            os.put((uchar)(0xc0u | (cp >> 6)));
            os.put((uchar)(0x80u | (cp & 0x3fu)));
        }
        else if (cp < 0x10000u)
        {
            os.put((uchar)(0xe0u | (cp >> 12)));
            os.put((uchar)(0x80u | ((cp >> 6) & 0x3fu)));
            os.put((uchar)(0x80u | (cp & 0x3fu)));
        }
        else
        {
            os.put((uchar)(0xf0u | (cp >> 18)));
            os.put((uchar)(0x80u | ((cp >> 12) & 0x3fu)));
            os.put((uchar)(0x80u | ((cp >> 6) & 0x3fu)));
            os.put((uchar)(0x80u | (cp & 0x3fu)));
        }
    }

    template <typename SiOutput>
    static inline void put(SiOutput& os, char16_t enc)
    {
        do_put(os, (cp_t)enc);
    }

    template <typename SiOutput>
    static inline void put(SiOutput& os, std::pair<char16_t, char16_t> enc)
    {
        cp_t v = (((cp_t)enc.first & 0x03ffu) << 10) | (enc.second & 0x03ffu);
        do_put(os, v + 0x10000u);
    }
};

// for in_mem
template <>
struct utf16_convert<unsigned char> : utf16_convert<char> {};

#ifdef __cpp_char8_t
template <>
struct utf16_convert<char8_t> : utf16_convert<char> {};
#endif

template <>
struct utf16_convert<char16_t>
{
    template <typename SiOutput>
    static inline void put(SiOutput& os, char16_t enc)
    {
        os.put(enc);
    }
    template <typename SiOutput>
    static inline void put(SiOutput& os, std::pair<char16_t, char16_t> enc)
    {
        os.put(enc.first);
        os.put(enc.second);
    }
};

template <>
struct utf16_convert<char32_t>
{
    template <typename SiOutput>
    static inline void put(SiOutput& os, char16_t enc)
    {
        os.put((char32_t)enc);
    }
    template <typename SiOutput>
    static inline void put(SiOutput& os, std::pair<char16_t, char16_t> enc)
    {
        os.put(((char32_t)enc.first << 16) | enc.second);
    }
};

inline bool is_high_surrogate(char16_t c)
{
    return (c & 0xfc00u) == 0xd800u;
}
inline bool is_low_surrogate(char16_t c)
{
    return (c & 0xfc00u) == 0xdc00u;
}
inline bool is_paired_unit(char16_t c)
{
    return c >= 0xd800u && c <= 0xdfffu;
}

// assumes leading \u was removed
template <typename SiInput, typename SiOutput>
inline bool utf_put_unescape(SiInput& is, SiOutput& os, tag::io_contiguous)
{
    using CharT = typename SiInput::char_type;
    
    auto* str = is.ipcur();
    auto size = is.ipend() - is.ipcur();

    if (size >= 10)
    {
        char16_t c1 = utf_unesc_unit(str, tag::io_contiguous{});
        if (c1 == UTF16_ERR) return false;

        if (is_high_surrogate(c1))
        {
            if (str[4] != 0x5c || str[5] != 0x75) // "\u"
                return false;

            char16_t c2 = utf_unesc_unit(str + 6, tag::io_contiguous{});
            if (c2 == UTF16_ERR || !is_low_surrogate(c2))
                return false;

            utf16_convert<CharT>::put(os, { c1, c2 });
            is.icommit(10);
            return true;
        }
        else if (!is_low_surrogate(c1)) // unpaired?
        {
            utf16_convert<CharT>::put(os, c1);
            is.icommit(4);
            return true;
        }
    }
    else if (size >= 4)
    {
        char16_t c1 = utf_unesc_unit(str, tag::io_contiguous{});
        if (c1 == UTF16_ERR || is_paired_unit(c1))
            return false;

        utf16_convert<CharT>::put(os, c1);
        is.icommit(4);
        return true;
    }
    return false;
}

// assumes leading \u was removed
template <typename SiInput, typename SiOutput>
inline bool utf_put_unescape(SiInput& is, SiOutput& os, tag::io_basic)
{
    using CharT = typename SiInput::char_type;

    char16_t c1 = utf_unesc_unit(is, tag::io_basic{});
    if (c1 == UTF16_ERR) return false;

    if (is_high_surrogate(c1))
    {
        if (!iutil::take(is, 0x5c) || !iutil::take(is, 0x75)) // "\u"
            return false;

        char16_t c2 = utf_unesc_unit(is, tag::io_basic{});
        if (c2 == UTF16_ERR || !is_low_surrogate(c2))
            return false;

        utf16_convert<CharT>::put(os, { c1, c2 });
        return true;
    }
    else if (!is_low_surrogate(c1)) // unpaired?
    {
        utf16_convert<CharT>::put(os, c1);
        return true;
    }
    return false;
}

}}}

#endif