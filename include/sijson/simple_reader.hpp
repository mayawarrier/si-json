
#ifndef SIJSON_SIMPLE_READER_HPP
#define SIJSON_SIMPLE_READER_HPP

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

#include "internal/config.hpp"
#include "internal/util.hpp"
#include "internal/buffers.hpp"
#include "internal/unicode.hpp"

#include "core.hpp"
#include "io_string.hpp"
#include "io_wrappers.hpp"


namespace sijson {

enum readflag : unsigned
{
    RDFLAG_none = 0x0,
    RDFLAG_str_copy = 0x1,
    RDFLAG_str_nodelim = 0x2
};

//
// A simple (low-level) JSON reader.
// 
// Does not maintain any internal state
// other than the input object.
//
template <typename Input>
class simple_reader
{
private:
    using InputTraits = input_traits<Input>;
    using SiInput = typename InputTraits::template wrapper_type_or<Input>;
    using InputKind = typename SiInput::input_kind;
    using InputSize = typename SiInput::size_type;
    using CharT = typename Input::char_type;
    using ChTraits = std::char_traits<CharT>;

    static constexpr bool IsNoThrowInput = 
        is_nothrow_input<SiInput, CharT>::value;

public:
    using char_type = CharT;
    using input_type = SiInput;

public:
    simple_reader(Input& in) :
        m_is(in)
    {}

    // Skip whitespace. 
    // Returns true if input has more characters.
    inline bool skip_ws(void) 
        noexcept(IsNoThrowInput)
    {
        while (!m_is.end() && iutil::is_ws(m_is.peek())) {
            m_is.take();
        }
        return !m_is.end();
    }

    // Get current token.
    inline sijson::token token(void) 
        noexcept(IsNoThrowInput)
    {
        if (!skip_ws())
            return TOKEN_eof;

        switch (m_is.peek())
        {
        case 0x7b: return TOKEN_start_object;   // '{'
        case 0x7d: return TOKEN_end_object;     // '}'
        case 0x5b: return TOKEN_start_array;    // '['
        case 0x5d: return TOKEN_end_array;      // ']'
        case 0x3a: return TOKEN_key_separator;  // ':'
        case 0x2c: return TOKEN_item_separator; // ','
        case 0x22: return TOKEN_string;         // '"'
        case 0x74:                              // 't'
        case 0x66: return TOKEN_boolean;        // 'f'
        case 0x6e: return TOKEN_null;           // 'n'
        case 0x2d: return TOKEN_number;         // '-'
        default:
            return iutil::is_digit(m_is.peek()) ?
                TOKEN_number : 
                TOKEN_invalid;
        }
    }

    // Read opening brace '{'.
    inline void read_start_object(void) { read_char(0x7b); }

    // Read closing brace '}'.
    inline void read_end_object(void) { read_char(0x7d); }

    // Read opening bracket '['.
    inline void read_start_array(void) { read_char(0x5b); }

    // Read closing bracket ']'.
    inline void read_end_array(void) { read_char(0x5d); }

    // Read key-value separator ':'.
    inline void read_key_separator(void) { read_char(0x3a); }

    // Read item separator ','.
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
    // precision = max digits desired to the right of the decimal point before 
    // rounding (the default is the maximum that can fit in a float).
    inline float read_float(int precision = std::numeric_limits<float>::max_digits10) 
    { 
        return read_floating<float>(precision, "float"); 
    }

    // Read a double.
    // precision = max digits desired to the right of the decimal point before 
    // rounding (the default is the maximum that can fit in a double).
    inline double read_double(int precision = std::numeric_limits<double>::max_digits10) 
    {
        return read_floating<double>(precision, "double"); 
    }

    // Read a number.
    // precision = max digits desired to the right of the decimal point before 
    // rounding (the default is the maximum that can fit in an sijson::number).
    inline number read_number(int precision = 
        std::numeric_limits<number::largest_fp_type>::max_digits10)
    {
        number value;
        bool any = skip_ws();
        InputSize err_offset = m_is.ipos();
        if (!any) goto fail;

        if (!do_read_number(m_is, precision, value)) goto fail;
        return value;
    fail:
        throw parse_error(err_offset, "expected number");
    }

    // Read boolean.
    inline bool read_bool(void)
    {
        bool any = skip_ws();
        InputSize err_offset = m_is.ipos();
        if (!any) goto fail;

        if (m_is.peek() == 0x74) // 't'
        {
            m_is.take();
            if (m_is.end() || m_is.take() != 0x72) goto fail; // 'r'
            if (m_is.end() || m_is.take() != 0x75) goto fail; // 'u'
            if (m_is.end() || m_is.take() != 0x65) goto fail; // 'e'
            return true;
        }
        else if (m_is.peek() == 0x66) // 'f'
        {
            m_is.take();
            if (m_is.end() || m_is.take() != 0x61) goto fail; // 'a'
            if (m_is.end() || m_is.take() != 0x6c) goto fail; // 'l'
            if (m_is.end() || m_is.take() != 0x73) goto fail; // 's'
            if (m_is.end() || m_is.take() != 0x65) goto fail; // 'e'
            return false;
        }
    fail:
        throw parse_error(err_offset, "expected boolean");
    }

    // Read null.
    inline void read_null(void)
    {
        bool any = skip_ws();
        InputSize err_offset = m_is.ipos();
        if (!any) goto fail;

        if (m_is.take() != 0x6e) goto fail; // 'n'
        if (m_is.end() || m_is.take() != 0x75) goto fail; // 'u'
        if (m_is.end() || m_is.take() != 0x6c) goto fail; // 'l'
        if (m_is.end() || m_is.take() != 0x6c) goto fail; // 'l'
        return;
    fail:
        throw parse_error(err_offset, "expected null");
    }

    // Read string. Throws on failure.
    template <typename StrAllocator = std::allocator<CharT>>
    inline std::basic_string<CharT, ChTraits, StrAllocator> read_string(void)
    {
        basic_out_stdstr<CharT, StrAllocator> os;

        skip_ws();
        error e = do_readstr(m_is, os);
        if (e) throw parse_error(m_is.ipos(), e);
        return std::move(os).str();
    }

    // Try to read string into given output object.
    //
    // flags may contain one or more of:
    // - RDFLAG_str_copy: copy string as-is (without unescaping).
    // - RDFLAG_str_nodelim: no quote delimiters. read input till end.
    //
    template <typename Output>
    inline error try_read_string_to(Output& out, unsigned flags = RDFLAG_none)
        noexcept(IsNoThrowInput && is_nothrow_output<iutil::remove_cvref_t<Output>, CharT>::value)
    {
        static_assert(std::is_same<typename Output::char_type, CharT>::value,
            "Output must have same char_type as input.");

        to_sijson_output_t<Output> os(out);

        skip_ws();

        switch (flags)
        {
        case RDFLAG_none:
            return do_readstr(m_is, os);

        case RDFLAG_str_copy:
            return do_copystr(m_is, os);

        case RDFLAG_str_nodelim:
            return do_readstr_noquotes(m_is, os);

        case RDFLAG_str_copy | RDFLAG_str_nodelim:
            return do_copystr_noquotes(m_is, os, InputKind{});

        default: return ERROR_bad_rdflags;
        }
    }

    // Read string into given output object.
    // Throws on failure.
    //
    // flags may contain one or more of:
    // - RDFLAG_str_copy: copy string as-is (without unescaping).
    // - RDFLAG_str_nodelim: no quote delimiters. read input till end.
    //
    template <typename Output>
    inline void read_string_to(Output& out, unsigned flags = RDFLAG_none)
    {
        error e = try_read_string_to(out, flags);
        if (e) throw parse_error(m_is.ipos(), e);
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

    // Get input object.
    inline input_type& in(void) noexcept { return m_is; }



private:
    static inline std::string c_to_errstr(CharT c)
    {
        std::string res = "expected ";
        if (c <= static_cast<CharT>(127)) 
        {
            res += (char)c; 
            res += '\'';
        } 
        else res.append("char " + 
                std::to_string(iutil::make_unsigned_t<CharT>(c)));
        return res;
    }

    template <typename UintT>
    static inline bool do_read_uintg(SiInput& is, UintT& out_value)
    {
        if (is.end() ||
            !iutil::is_digit(is.peek()) ||
            is.peek() == 0x2d) // '-'
            return false;

        out_value = 0;
        while (!is.end() && iutil::is_digit(is.peek()))
        {
            UintT old = out_value;
            out_value = 10 * out_value + ((UintT)is.peek() - (UintT)0x30); // '0'
            if (out_value < old) return false; // overflow
            is.take();
        }
        return true;
    }

    template <typename IntT, IntT Lbound, IntT Ubound>
    static inline bool do_read_intg(SiInput& is, IntT& out_value)
    {
        bool neg = is.peek() == 0x2d; // '-'
        if (neg) is.take();

        typename std::make_unsigned<IntT>::type uvalue;
        if (!do_read_uintg(is, uvalue)) return false;
        if (neg) {
            if (uvalue > iutil::absu(Lbound)) 
                return false;
        }
        else if (uvalue > iutil::absu(Ubound)) 
            return false;

        out_value = neg ? iutil::uneg<IntT, Lbound, Ubound>(uvalue) : (IntT)uvalue;
        return true;
    }

    template <typename SiOutput>
    static inline void take_numstr(SiInput& is, SiOutput& os)
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

    template <bool CheckNanInfHex = true, typename SiContiguousOutput, typename FloatT>
    static inline bool do_read_floating(SiContiguousOutput& numstr, int precision, FloatT& out_value)
    {
        if (numstr.opos() == 0)
            return false;

        auto strdata = sijson::outdata(numstr);
        auto* str_begin = strdata.begin;
        auto* str_end = strdata.end;

        bool neg = false;
        if (CheckNanInfHex)
        {
            // skip '-'
            if (*str_begin == 0x2d) {
                neg = true; str_begin++;
                // only sign?
                if (numstr.opos() == 1)
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

    static inline bool out_inumber(std::uintmax_t value, bool neg, number& out_value)
    {
        if (neg) {
            if (value > iutil::absu(std::numeric_limits<std::intmax_t>::min()))
                return false;
            out_value = iutil::uneg<std::intmax_t>(value);
        }
        else out_value = value;
        return true;
    }

    static inline number to_fnumber(double value)
    {
        // todo: with -ffastmath/fp:fast, this may always select float over double
        if (value > std::numeric_limits<float>::max() || value != (float)value)
            return { value };
        else return { (float)value };
    }

    static inline bool do_read_number(SiInput& is, int fp_precision, number& out_value)
    {
        // floating point value
        CharT fpstrbuf[iutil::max_chars10<number::largest_fp_type>::value];
        basic_out_strspan<CharT, tag::throw_on_overflow> fpstr(fpstrbuf);

        bool neg = is.peek() == 0x2d; // '-'
        if (neg) fpstr.put(is.take());

        if (is.end() || !iutil::is_digit(is.peek()))
            return false;

        std::uintmax_t old_int_v, int_v = 0;
        while (!is.end() && iutil::is_digit(is.peek()))
        {
            old_int_v = int_v;
            auto c = is.take(); fpstr.put(c);
            int_v = 10 * int_v + ((std::uintmax_t)c - 0x30); // '0'
            if (int_v < old_int_v) return false; // overflow
        }
        if (is.end() || iutil::is_ws(is.peek()))
            return out_inumber(int_v, neg, out_value);

        switch (is.peek())
        {
        case 0x2c: case 0x5d: case 0x7d: // ',', ']', '}'
            return out_inumber(int_v, neg, out_value);

        case 0x2e: case 0x65: case 0x45: // '.', 'e', 'E'
        {
            double float_v;
            take_numstr(is, fpstr); // remaining
            bool success = do_read_floating<false>(fpstr, fp_precision, float_v);
            if (success) out_value = to_fnumber(float_v);
            return success;
        }

        default: return false;
        }
    }

    template <typename SiOutput>
    static inline bool unescape(SiInput& is, SiOutput& os)
    {
        // row = 16 chars
        // \b, \f, \n, \r, \t, /, \, "
        static constexpr char ctrl_lut[]
        {
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0, 0x22, 0,0,0,0,0,0,0,0,0,0,0,0, 0x2f,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0, 0x5c, 0,0,0,
            0,0, 0x8, 0,0,0, 0xc, 0,0,0,0,0,0,0, 0xa, 0,
            0,0, 0xd, 0, 0x9, 0,0,0,0,0,0,0,0,0,0,0
        };

        is.take(); // skip '\'
        if (is.end())
            return false;

        auto c = is.peek();
        if (c <= static_cast<CharT>(127) && 
            ctrl_lut[ChTraits::to_int_type(c)] != 0)
        {
            os.put(ctrl_lut[ChTraits::to_int_type(c)]);
            is.take();
        }
        else if (c == 0x75) // 'u' (unicode)
        {
            is.take();
            if (!iutil::utf_put_unescape(is, os, InputKind{}))
                return false;
        }
        else return false;
        return true;
    }

    template <typename SiOutput>
    static inline error do_readstr(SiInput& is, SiOutput& os)
    {
        if (is.end() || is.peek() != 0x22) // '"'
            return ERROR_str_delim;

        is.take(); // skip '"'
        while (!is.end() && is.peek() != 0x22) // '"'
        {
            if (is.peek() != 0x5c) // '\'
                os.put(is.take());
            else if (!unescape(is, os))
                return ERROR_str_escape;
        }
        if (is.end()) 
            return ERROR_str_delim;
        is.take(); // '"'

        return ERROR_none;
    }

    template <typename SiOutput>
    static inline error do_copystr(SiInput& is, SiOutput& os)
    {
        if (is.end() || is.peek() != 0x22) // '"'
            return ERROR_str_delim;

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
                    return ERROR_str_escape;

                if (is.peek() == 0x75)
                {
                    // todo unicode
                    throw parse_error(is.ipos(), 
                        "Unicode unimplemented for RDFLAG_str_copy");
                }
                os.put(is.take());
            }
        }

        if (is.end())
            return ERROR_str_delim;
        is.take(); // '"'
        os.put(0x22); // '"'

        return ERROR_none;
    }

    template <typename SiOutput, typename MyInputKind>
    static inline void copy_unesc_str_noquotes(SiInput&, SiOutput&, MyInputKind) {}

    template <typename SiOutput>
    static inline void copy_unesc_str_noquotes(SiInput& is, SiOutput& os, tag::io_contiguous) 
    {
        auto n_in = sijson::inrem(is);
        auto* escp = ChTraits::find(is.ipcur(), n_in, 0x5c); // '\'

        InputSize n_out = escp ? (InputSize)(escp - is.ipcur()) : n_in;
        os.put_n(is.ipcur(), static_cast<std::size_t>(n_out));
        is.icommit(n_out);
    }

    static constexpr bool did_copy_unesc_str_noquotes()
    {
        return std::is_same<InputKind, tag::io_contiguous>::value;
    }

    template <typename SiOutput>
    static inline error do_readstr_noquotes(SiInput& is, SiOutput& os)
    {
        while (true)
        {
            copy_unesc_str_noquotes(is, os, InputKind{});
            if (is.end()) break;

            if (!did_copy_unesc_str_noquotes() && is.peek() != 0x5c) { // '\'
                os.put(is.take());
            }  
            else if (!unescape(is, os))
                return ERROR_str_escape;
        }
        return ERROR_none;
    }

    template <typename SiOutput>
    static inline error do_copystr_noquotes(SiInput& is, SiOutput& os, tag::io_contiguous)
    {
        auto n = (std::size_t)(is.ipend() - is.ipcur());
        os.put_n(is.ipcur(), n);
        is.icommit(n);
        return ERROR_none;
    }

    template <typename SiOutput>
    static inline error do_copystr_noquotes(SiInput& is, SiOutput& os, tag::io_basic)
    {
        while (!is.end())
            os.put(is.take());
        return ERROR_none;
    }

    template <typename SiOutput>
    static inline error do_copystr_noquotes(SiInput& is, SiOutput& os, tag::io_buffered)
    {
        return do_copystr_noquotes(is, os, tag::io_basic{});
    }

    template <typename IntT,
        IntT Lbound = std::numeric_limits<IntT>::min(),
        IntT Ubound = std::numeric_limits<IntT>::max()>
    inline IntT read_intg(const char* type_name = nullptr)
    {
        IntT value;
        if (!skip_ws()) goto fail;
        if (!do_read_intg<IntT, Lbound, Ubound>(m_is, value)) goto fail;
        return value;
    fail:
        throw parse_error(m_is.ipos(), 
            type_name ? type_name : "expected signed integral");
    }

    template <typename UintT,
        UintT Ubound = std::numeric_limits<UintT>::max()>
    inline UintT read_uintg(const char* type_name = nullptr)
    {
        UintT value;
        if (!skip_ws()) goto fail;
        if (!do_read_uintg(m_is, value)) goto fail;
        if (value > Ubound) goto fail;
        return value;
    fail:
        throw parse_error(m_is.ipos(), 
            type_name ? type_name : "expected unsigned integral");
    }

    template <typename FloatT>
    inline FloatT read_floating(
        int precision = std::numeric_limits<FloatT>::max_digits10, const char* type_name = nullptr)
    {
        bool any = skip_ws();
        std::size_t err_offset = m_is.ipos();
        if (!any) goto fail;

        FloatT value;
        {
            CharT strbuf[iutil::max_chars10<FloatT>::value];
            basic_out_strspan<CharT, tag::throw_on_overflow> numstr(strbuf);
            take_numstr(m_is, numstr);

            if (!do_read_floating(numstr, precision, value)) 
                goto fail;
        }
        return value;
    fail:
        throw parse_error(err_offset, 
            type_name ? type_name : "expected floating point");
    }

    inline void read_char(CharT c)
    {
        if (!skip_ws() || !iutil::take(m_is, c))
            throw parse_error(m_is.ipos(), c_to_errstr(c));
    }

    struct read_t_impl
    {
        template <typename T, iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value> = 0>
        static inline T read(simple_reader& r) { return r.read_intg<T>(); }

        template <typename T, iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value> = 0>
        static inline T read(simple_reader& r) { return r.read_uintg<T>(); }

        template <typename T, iutil::enable_if_t<std::is_floating_point<T>::value> = 0>
        static inline T read(simple_reader& r) { return r.read_floating<T>(); }

        template <typename T, iutil::enable_if_same_t<T, number> = 0>
        static inline T read(simple_reader& r) { return r.read_number(); }

        template <typename T, iutil::enable_if_same_t<T, bool> = 0>
        static inline T read(simple_reader& r) { return r.read_bool(); }

        template <typename T, iutil::enable_if_t<iutil::is_instance_of_basic_string<T, CharT, ChTraits>::value> = 0>
        static inline T read(simple_reader& r) { return r.read_string<typename T::allocator_type>(); }

        template <typename T, iutil::enable_if_same_t<T, std::nullptr_t> = 0>
        static inline T read(simple_reader& r) { r.read_null(); return nullptr; }
    };

private:
    typename InputTraits::template wrapper_type_or<Input&> m_is;
};

}

#endif