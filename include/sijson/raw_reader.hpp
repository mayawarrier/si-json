
#ifndef SIJSON_RAW_READER_HPP
#define SIJSON_RAW_READER_HPP

#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <limits>
#include <type_traits>
#include <ios>
#include <istream>
#include <locale>
#include <memory>
#include <stdexcept>

#include "internal/util.hpp"
#include "internal/buffers.hpp"

#include "core.hpp"
#include "concepts.hpp"
#include "number.hpp"
#include "stringstream.hpp"
#include "stream_wrappers.hpp"


namespace sijson {

// Basic JSON reader.
// Does not maintain any internal state - all
// operations are performed on the stream only.
template <typename Istream>
class raw_reader
{
private:
    using IstreamTraits = istream_traits<Istream>;
    using SiIstream = typename IstreamTraits::template wrapper_type_or<Istream>;
    using Streamsize = typename SiIstream::streamsize_type;
    using CharT = typename Istream::char_type;
    using ChTraits = std::char_traits<CharT>;

    enum read_string_result : int
    {
        SREADE_BADESCAPE = -2,
        SREADE_NOTASTRING = -1,
        SREADE_SUCCESS = 0
    };

public:
    using char_type = CharT;
    using stream_type = SiIstream;

    enum read_string_flag : unsigned
    {
        SREADF_NONE = 0x0,
        SREADF_NOUNESCAPE = 0x1,
        SREADF_NOQUOTES = 0x2
    };

    // True if Istream does not implement sijson's istream concept 
    // and was wrapped by a stream wrapper (see stream_wrappers.hpp).
    // Member typedefs reflect the properties of the wrapper.
    static constexpr bool stream_is_wrapped = IstreamTraits::is_wrapped;

public:
    raw_reader(Istream& stream) :
        m_stream(stream)
    {}

    // Get next unread token, skipping any whitespace.
    inline sijson::token token(void)
    {
        if (!iutil::skip_ws(m_stream))
            return TOKEN_eof;

        switch (m_stream.peek())
        {
        case 0x7b: return TOKEN_begin_object;   // '{'
        case 0x7d: return TOKEN_end_object;     // '}'
        case 0x5b: return TOKEN_begin_array;    // '['
        case 0x5d: return TOKEN_end_array;      // ']'
        case 0x3a: return TOKEN_key_separator;  // ':'
        case 0x2c: return TOKEN_item_separator; // ','
        case 0x22: return TOKEN_string;         // '"'
        case 0x74:                              // 't'
        case 0x66: return TOKEN_boolean;        // 'f'
        case 0x6e: return TOKEN_null;           // 'n'
        case 0x2d: return TOKEN_number;         // '-'
        default:
            if (iutil::is_digit(m_stream.peek()))
                return TOKEN_number;
            else
                throw iutil::parse_error_exp(m_stream.inpos(), "token");
        }
    }

    inline void read_start_object(void) { read_char(0x7b); }
    inline void read_end_object(void) { read_char(0x7d); }
    inline void read_start_array(void) { read_char(0x5b); }
    inline void read_end_array(void) { read_char(0x5d); }
    inline void read_key_separator(void) { read_char(0x3a); }
    inline void read_item_separator(void) { read_char(0x2c); }

    // Read a 32-bit signed integral number.
    inline std::int_least32_t read_int32(void) 
    { 
        return read_intg<std::int_least32_t, iutil::int32_min, iutil::int32_max>("int32"); 
    }

    // Read a 64-bit signed integral number.
    inline std::int_least64_t read_int64(void) 
    { 
        return read_intg<std::int_least64_t, iutil::int64_min, iutil::int64_max>("int64"); 
    }

    // Read a 32-bit unsigned integral number.
    inline std::uint_least32_t read_uint32(void) 
    { 
        return read_uintg<std::uint_least32_t, iutil::uint32_max>("uint32"); 
    }

    // Read a 64-bit unsigned integral number.
    inline std::uint_least64_t read_uint64(void) 
    { 
        return read_uintg<std::uint_least64_t, iutil::uint64_max>("uint64"); 
    }

    // Read a float.
    // precision: max digits required to the right of the decimal point before 
    // rounding (by default this is the maximum representable by float).
    inline float read_float(int precision = std::numeric_limits<float>::max_digits10) 
    { 
        return read_floating<float>(precision, "float"); 
    }

    // Read a double.
    // precision: max digits required to the right of the decimal point before 
    // rounding (by default this is the maximum representable by double).
    inline double read_double(int precision = std::numeric_limits<double>::max_digits10) 
    {
        return read_floating<double>(precision, "double"); 
    }

    // Read a number.
    // precision: max digits required to the right of the decimal point before 
    // rounding (by default this is the maximum representable by sijson::number).
    inline number read_number(int precision = 
        std::numeric_limits<number::largest_fp_type>::max_digits10)
    {
        number value;
        std::size_t error_offset;
        if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

        if (!do_read_number(m_stream, precision, value)) goto fail;
        return value;
    fail:
        throw iutil::parse_error_exp(error_offset, "number");
    }

    // Read boolean.
    inline bool read_bool(void)
    {
        std::size_t error_offset;
        if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

        if (m_stream.peek() == 0x74) // 't'
        {
            m_stream.take();
            if (m_stream.end() || m_stream.take() != 0x72) goto fail; // 'r'
            if (m_stream.end() || m_stream.take() != 0x75) goto fail; // 'u'
            if (m_stream.end() || m_stream.take() != 0x65) goto fail; // 'e'
            return true;
        }
        else if (m_stream.peek() == 0x66) // 'f'
        {
            m_stream.take();
            if (m_stream.end() || m_stream.take() != 0x61) goto fail; // 'a'
            if (m_stream.end() || m_stream.take() != 0x6c) goto fail; // 'l'
            if (m_stream.end() || m_stream.take() != 0x73) goto fail; // 's'
            if (m_stream.end() || m_stream.take() != 0x65) goto fail; // 'e'
            return false;
        }
    fail:
        throw iutil::parse_error_exp(error_offset, "bool");
    }

    // Read null.
    inline void read_null(void)
    {
        std::size_t error_offset;
        if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

        if (m_stream.take() != 0x6e) goto fail; // 'n'
        if (m_stream.end() || m_stream.take() != 0x75) goto fail; // 'u'
        if (m_stream.end() || m_stream.take() != 0x6c) goto fail; // 'l'
        if (m_stream.end() || m_stream.take() != 0x6c) goto fail; // 'l'
        return;
    fail:
        throw iutil::parse_error_exp(error_offset, "null");
    }

    // Read string. String is unescaped.
    template <typename StrAllocator = std::allocator<CharT>>
    inline std::basic_string<CharT, ChTraits, StrAllocator> read_string(void)
    {
        basic_ostdstrstream<CharT, StrAllocator> os;
        do_read_string(m_stream, os);
        return std::move(os).str();
    }

    // Read string into output stream.
    //
    // flags may contain one or more of:
    // - SREADF_NOUNESCAPE: do not unescape output.
    // - SREADF_NOQUOTES: do not treat quotes as delimiters
    //   (input stream will be read till the end).
    //
    template <typename Ostream>
    inline void read_string_into(Ostream& stream, unsigned flags = SREADF_NONE)
    {
        sijson_ostream_t<Ostream> os(stream);
        using is_icontiguous = iutil::to_bool_t<is_contiguous_istream<SiIstream, CharT>>;

        switch (flags)
        {
        case SREADF_NONE:
            do_read_string(m_stream, os);
            break;

        case SREADF_NOUNESCAPE:
            do_copy_string(m_stream, os);
            break;

        case SREADF_NOQUOTES:
            do_read_string_noquotes(m_stream, os, is_icontiguous{});
            break;

        case SREADF_NOUNESCAPE | SREADF_NOQUOTES:
            do_copy_string_noquotes(m_stream, os, is_icontiguous{});          
            break;

        default:
#if SIJSON_USE_LOGIC_ERRORS
            throw std::invalid_argument(std::string(__func__) + ": invalid flags.");
#elif defined(__cpp_lib_unreachable)
            std::unreachable();
#else
            assert(false);
            break;
#endif
        }
    }

    // Read value.
    // 
    // T may be any integral or floating point
    // type, sijson::number, std::nullptr_t, bool,
    // or a specialization of std::basic_string<>.
    //
    template <typename T>
    inline T read(void)
    {
        return read_t_impl::template read<T>(*this);
    }

    // Get stream.
    inline stream_type& stream(void) noexcept { return m_stream; }

private:
    template <typename Ostream>
    static inline void take_numstr(SiIstream& is, Ostream& os)
    {
        while (!is.end() && !iutil::is_ws(is.peek()))
        {
            switch (is.peek())
            {
            case 0x2c: case 0x5d: case 0x7d: // ',', ']', '}'
                return;
            default: os.put(is.take());
                break;
            }
        }
    }

    template <typename UintT>
    static inline bool do_read_uintg(SiIstream& stream, UintT& out_value)
    {
        if (stream.end() ||
            !iutil::is_digit(stream.peek()) ||
            stream.peek() == 0x2d) // '-'
            return false;

        out_value = 0;
        while (!stream.end() && iutil::is_digit(stream.peek()))
        {
            UintT old = out_value;
            out_value = 10 * out_value + (stream.peek() - (CharT)0x30); // '0'
            if (out_value < old) return false; // overflow
            stream.take();
        }
        return true;
    }

    template <typename IntT, IntT Lbound, IntT Ubound>
    static inline bool do_read_intg(SiIstream& stream, IntT& out_value)
    {
        bool neg = stream.peek() == 0x2d; // '-'
        if (neg) stream.take();

        typename std::make_unsigned<IntT>::type uvalue;
        if (!do_read_uintg(stream, uvalue)) return false;
        if (neg) {
            if (uvalue > iutil::absu(Lbound)) 
                return false;
        }
        else if (uvalue > iutil::absu(Ubound)) 
            return false;

        out_value = neg ? iutil::uneg<IntT, Lbound, Ubound>(uvalue) : (IntT)uvalue;
        return true;
    }

    template <bool CheckNanInfHex = true, typename SiContiguousOstream, typename FloatT>
    static inline bool do_read_floating(SiContiguousOstream& numstream, int precision, FloatT& out_value)
    {
        if (numstream.outpos() == 0)
            return false;

        auto strdata = sijson::outdata(numstream);
        auto* str_begin = strdata.begin;
        auto* str_end = strdata.end;

        bool neg = false;
        if (CheckNanInfHex)
        {
            // skip '-'
            if (*str_begin == 0x2d) {
                neg = true; str_begin++;
                // only sign?
                if (numstream.outpos() == 1)
                    return false;
            }

            auto ci_equal = [](CharT lhs, CharT rhs) {
                return rhs == iutil::to_lower(lhs);
            };

            static constexpr CharT shex[] = { 0x30, 0x78 }; // "0x"
            static constexpr CharT snan[] = { 0x6e, 0x61, 0x6e }; // "nan"
            static constexpr CharT sinf[] = { 0x69, 0x6e, 0x66 }; // "inf"

            if (iutil::starts_with(str_begin, str_end, shex, 2, ci_equal) ||
                iutil::starts_with(str_begin, str_end, snan, 3, ci_equal) ||
                iutil::starts_with(str_begin, str_end, sinf, 3, ci_equal))
                return false;
        }

        internal::memspanbuf streambuf(str_begin,
            (std::streamsize)(str_end - str_begin), std::ios_base::in);
        std::istream sstream(&streambuf);
        sstream.imbue(std::locale::classic()); // make decimal point '.'
        sstream.precision(precision);
        sstream >> out_value;

        if (CheckNanInfHex && neg)
            out_value *= (FloatT)(-1);

        return !sstream.fail();
    }

    static inline bool do_read_number(SiIstream& stream, int fp_precision, number& out_value)
    {
        auto out_inumber = [&](std::uintmax_t value, bool neg) -> bool
        {
            if (neg) {
                if (value > iutil::absu(std::numeric_limits<std::intmax_t>::min()))
                    return false;
                out_value = iutil::uneg<std::intmax_t>(value);
            }
            else out_value = value;
            return true;
        };

        auto to_fnumber = [](double value) -> number
        {
            // todo: with -ffastmath/fp:fast, this may always select float over double
            if (value > std::numeric_limits<float>::max() || value != (float)value)
                return { value };
            else return { (float)value };
        };

        // floating point value
        CharT fpstrbuf[iutil::max_chars10<number::largest_fp_type>::value];
        basic_ostrspanstream<CharT, tag::throw_on_overflow> fpstream(fpstrbuf);

        bool neg = stream.peek() == 0x2d; // '-'
        if (neg) fpstream.put(stream.take());

        if (stream.end() || !iutil::is_digit(stream.peek()))
            return false;

        std::uintmax_t old_int_v, int_v = 0;
        while (!stream.end() && iutil::is_digit(stream.peek()))
        {
            old_int_v = int_v;
            auto c = stream.take(); fpstream.put(c);
            int_v = 10 * int_v + (c - (CharT)0x30); // '0'
            if (int_v < old_int_v) return false; // overflow
        }
        if (stream.end() || iutil::is_ws(stream.peek()))
            return out_inumber(int_v, neg);

        switch (stream.peek())
        {
        case 0x2c: case 0x5d: case 0x7d: // ',', ']', '}'
            return out_inumber(int_v, neg);

        case 0x2e: case 0x65: case 0x45: // '.', 'e', 'E'
        {
            double float_v;
            take_numstr(stream, fpstream); // remaining
            bool success = do_read_floating<false>(fpstream, fp_precision, float_v);
            if (success) out_value = to_fnumber(float_v);
            return success;
        }

        default: return false;
        }
    }

    template <typename SiOstream>
    static inline int take_unescape(SiIstream& is, SiOstream& os)
    {
        if (is.peek() != 0x5c) // '\'
            os.put(is.take());
        else
        {
            is.take(); // '\'
            if (is.end())
                return SREADE_BADESCAPE;

            switch (is.peek())
            {
            case 0x62: os.put(0x08); break; // '\b'
            case 0x66: os.put(0x0c); break; // '\f'
            case 0x6e: os.put(0x0a); break; // '\n'
            case 0x72: os.put(0x0d); break; // '\r'
            case 0x74: os.put(0x09); break; // '\t'
            case 0x22: os.put(0x22); break; // '"'
            case 0x2f: os.put(0x2f); break; // '/'
            case 0x75:                      // 'u'
            {
                throw iutil::parse_error(is.inpos(),
                    "Unicode escape sequences are not supported yet.");
            }
            default: 
                return SREADE_BADESCAPE;
            }
            is.take();
        }
        return SREADE_SUCCESS;
    }

    template <typename SiOstream>
    static inline int do_read_string(SiIstream& is, SiOstream& os)
    {
        if (!iutil::skip_ws(is) || is.peek() != 0x22) // '"'
            return SREADE_NOTASTRING;

        is.take(); // '"'
        while (!is.end() && is.peek() != 0x22) // '"'
        {
            int err = take_unescape(is, os);
            if (err) return err;
        }
        if (is.end()) 
            return SREADE_NOTASTRING;
        is.take(); // '"'

        return SREADE_SUCCESS;
    }

    template <typename SiOstream>
    static inline int do_copy_string(SiIstream& is, SiOstream& os)
    {
        if (!iutil::skip_ws(is) || is.peek() != 0x22) // '"'
            return SREADE_NOTASTRING;

        is.take(); // '"'
        os.put(0x22); // '"'
        while (!is.end() && is.peek() != 0x22) // '"'
        {
            if (is.peek() != 0x5c) // '\'
                os.put(is.take());
            else
            {
                os.put(is.take());
                if (is.end())
                    return SREADE_BADESCAPE;
                os.put(is.take());
            }
        }

        if (is.end())
            return SREADE_NOTASTRING;
        is.take(); // '"'
        os.put(0x22); // '"'

        return SREADE_SUCCESS;
    }

    template <typename SiOstream>
    static inline int do_read_string_noquotes(SiIstream& is, SiOstream& os, 
        std::true_type /* is_istream_contiguous = true */)
    {
        iutil::skip_ws(is);
        if (is.end()) return SREADE_SUCCESS;

        const CharT* escp = nullptr;
        while ((escp = ChTraits::find(is.inpcur(), 
            (std::size_t)(is.inpend() - is.inpcur()), 0x5c)) != nullptr) // '\'
        {
            // copy everything before escape
            os.put_n(is.inpcur(), (std::size_t)(escp - is.inpcur())); 
            escp++;

            if (escp == is.inpend())
            {
                is.incommit((Streamsize)(escp - is.inpcur()));
                return SREADE_BADESCAPE;
            }

            switch (*escp)
            {
            case 0x62: os.put(0x08); break; // '\b'
            case 0x66: os.put(0x0c); break; // '\f'
            case 0x6e: os.put(0x0a); break; // '\n'
            case 0x72: os.put(0x0d); break; // '\r'
            case 0x74: os.put(0x09); break; // '\t'
            case 0x22: os.put(0x22); break; // '"'
            case 0x2f: os.put(0x2f); break; // '/'
            case 0x75:                      // 'u'
            {
                is.incommit((Streamsize)(escp - is.inpcur()));
                throw iutil::parse_error(is.inpos(),
                    "Unicode escape sequences are not supported yet.");
            }
            default: 
                is.incommit((Streamsize)(escp - is.inpcur()));
                return SREADE_BADESCAPE;
            }

            is.incommit((Streamsize)(escp - is.inpcur()) + 1);
        }

        // copy remaining
        os.put_n(is.inpcur(), (std::size_t)(is.inpend() - is.inpcur()));
        return SREADE_SUCCESS;
    }

    template <typename SiOstream>
    static inline int do_read_string_noquotes(SiIstream& is, SiOstream& os, 
        std::false_type /* is_istream_contiguous = false */)
    {
        iutil::skip_ws(is);
        while (!is.end())
        {
            int err = take_unescape(is, os);
            if (err) return err;
        }
        return SREADE_SUCCESS;
    }

    template <typename SiOstream>
    static inline void do_copy_string_noquotes(SiIstream& is, SiOstream& os, 
        std::true_type /* is_istream_contiguous = true */)
    {
        iutil::skip_ws(is);
        auto n = (std::size_t)(is.inpend() - is.inpcur());
        os.put_n(is.inpcur(), n);
        is.incommit(n);
    }

    template <typename SiOstream>
    static inline void do_copy_string_noquotes(SiIstream& is, SiOstream& os, 
        std::false_type /* is_istream_contiguous = false */)
    {
        iutil::skip_ws(is);
        while (!is.end())
            os.put(is.take());
    }

    template <typename IntT,
        IntT Lbound = std::numeric_limits<IntT>::min(),
        IntT Ubound = std::numeric_limits<IntT>::max()>
    inline IntT read_intg(const char* type_name = nullptr)
    {
        IntT value;
        if (!iutil::skip_ws(m_stream)) goto fail;
        if (!do_read_intg<IntT, Lbound, Ubound>(m_stream, value)) goto fail;
        return value;
    fail:
        throw iutil::parse_error_exp(m_stream.inpos(), type_name ? type_name : "signed integral");
    }

    template <typename UintT,
        UintT Ubound = std::numeric_limits<UintT>::max()>
    inline UintT read_uintg(const char* type_name = nullptr)
    {
        UintT value;
        if (!iutil::skip_ws(m_stream)) goto fail;
        if (!do_read_uintg(m_stream, value)) goto fail;
        if (value > Ubound) goto fail;
        return value;
    fail:
        throw iutil::parse_error_exp(m_stream.inpos(), type_name ? type_name : "unsigned integral");
    }

    template <typename FloatT>
    inline FloatT read_floating(int precision = std::numeric_limits<FloatT>::max_digits10, const char* type_name = nullptr)
    {
        std::size_t error_offset;
        if (!iutil::skip_ws(m_stream, error_offset)) goto fail;

        FloatT value;
        {
            CharT strbuf[iutil::max_chars10<FloatT>::value];
            basic_ostrspanstream<CharT, tag::throw_on_overflow> numstream(strbuf);
            take_numstr(m_stream, numstream);

            if (!do_read_floating(numstream, precision, value)) 
                goto fail;
        }
        return value;
    fail:
        throw iutil::parse_error_exp(error_offset, type_name ? type_name : "floating point");
    }

    inline void read_char(CharT expected)
    {
        if (!iutil::skip_ws(m_stream)) goto fail;
        if (m_stream.peek() != expected) goto fail;
        m_stream.take();
        return;
    fail:
        throw iutil::parse_error_exp(m_stream.inpos(),
            std::string("'") + expected + '\'');
    }

    inline void throw_sread_err(read_string_result err)
    {
        switch (err)
        {
        case SREADE_BADESCAPE:
            throw iutil::parse_error(m_stream.pos(), "Invalid escape");
        case SREADE_NOTASTRING:
            throw iutil::parse_error_exp(m_stream.pos(), "string");
        default: break;
        }
    }

    struct read_t_impl
    {
        template <typename T, iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value> = 0>
        static inline T read(raw_reader& r) { return r.read_intg<T>(); }

        template <typename T, iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value> = 0>
        static inline T read(raw_reader& r) { return r.read_uintg<T>(); }

        template <typename T, iutil::enable_if_t<std::is_floating_point<T>::value> = 0>
        static inline T read(raw_reader& r) { return r.read_floating<T>(); }

        template <typename T, iutil::enable_if_same_t<T, number> = 0>
        static inline T read(raw_reader& r) { return r.read_number(); }

        template <typename T, iutil::enable_if_same_t<T, bool> = 0>
        static inline T read(raw_reader& r) { return r.read_bool(); }

        template <typename T, iutil::enable_if_t<iutil::is_instance_of_basic_string<T, CharT, ChTraits>::value> = 0>
        static inline T read(raw_reader& r) { return r.read_string<typename T::allocator_type>(); }

        template <typename T, iutil::enable_if_same_t<T, std::nullptr_t> = 0>
        static inline T read(raw_reader& r) { r.read_null(); return nullptr; }
    };

private:
    typename IstreamTraits::template wrapper_type_or<Istream&> m_stream;
};

}

#endif