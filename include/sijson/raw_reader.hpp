
#ifndef SIJSON_RAW_READER_HPP
#define SIJSON_RAW_READER_HPP

#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <limits>
#include <type_traits>
#include <memory>
#include <stdexcept>

#include "internal/unicode.hpp"
#include "internal/core.hpp"

#include "concepts.hpp"
#include "io_string.hpp"
#include "io_wrappers.hpp"

#if SIJSON_USE_FASTFLOAT
#include "fast_float/fast_float.h"
#endif

#if SIJSON_SIMD
#include <intrin.h>
#endif

namespace sijson {

enum rdflag : unsigned
{
    RDFLAG_none = 0x0,
    RDFLAG_str_copy = 0x1,
    RDFLAG_str_only = 0x2,
    RDFLAG_allow_nan_inf = 0x4
};

//
// Low-level JSON reader.
// 
// Input type can be:
// - std input stream (std::stringstream, std::filestream, etc),
// - sijson input object (in_str, in_file, etc),
// - custom input type (see concepts.hpp).
// 
// - Options is an sijson::options that may contain:
//   * OPT_no_whitespace: assume input contains no whitespace.
// 
// - Two APIs are provided:
//   * a throwing API, which throws sijson::parse_error on failure.
//   * a non-throwing API, which returns error codes.
//
template <typename Input, typename Options = void>
class raw_reader
{
private:
    using MyInput = to_si_input_t<Input>;
    using InputKind = typename MyInput::input_kind;
    using InputSize = typename MyInput::size_type;
    using CharT = typename MyInput::char_type;
    using ChTraits = std::char_traits<CharT>;

    template <typename Output>
    using IsNoThrowOutput = is_nothrow_output<to_si_output_t<iutil::remove_cvref_t<Output>>, CharT>;

    static constexpr bool IsNoThrowInput = is_nothrow_input<MyInput, CharT>::value;
    static constexpr bool IsNoThrowContiguousInput = is_nothrow_contiguous_input<MyInput, CharT>::value;
    
    // at least basic input
    static_assert(is_basic_input<MyInput, CharT>::value, "Incompatible input type.");

public:
    using char_type = CharT;
    using input_type = MyInput;

    // True if reader assumes no whitespace is present in input.
    static constexpr bool assumes_no_ws() { return has_opt<Options, no_whitespace>(); }

public:
    raw_reader(Input& in)
        noexcept(noexcept(wrap_input_t<Input>(in))) :
        m_is(in)
    {}

    // Skip whitespace. 
    // Returns true if input has more characters.
    inline bool skip_ws(void) noexcept(IsNoThrowInput)
    {
        while (!m_is.end() && iutil::is_ws(m_is.peek())) {
            m_is.take();
        }
        return !m_is.end();
    }

    // Get current token.
    inline sijson::token token(void) noexcept(IsNoThrowInput)
    {
        bool eof = assumes_no_ws() ? m_is.end() : !skip_ws();
        if (eof) return TOKEN_eof;

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
    inline void read_start_object(void) { read_char(0x7b, ERROR_token_start_object); }

    // Read closing brace '}'.
    inline void read_end_object(void) { read_char(0x7d, ERROR_token_end_object); }

    // Read opening bracket '['.
    inline void read_start_array(void) { read_char(0x5b, ERROR_token_start_array); }

    // Read closing bracket ']'.
    inline void read_end_array(void) { read_char(0x5d, ERROR_token_end_array); }

    // Read key-value separator ':'.
    inline void read_key_separator(void) { read_char(0x3a, ERROR_token_key_sep); }

    // Read item separator ','.
    inline void read_item_separator(void) { read_char(0x2c, ERROR_token_item_sep); }


    // Read a 32-bit signed integer.
    // Throws parse_error on failure.
    inline std::int_least32_t read_int32(void)
    {
        return read_intg<std::int_least32_t, iutil::int32_min, iutil::int32_max>("int32");
    }

    // Read a 64-bit signed integer.
    // Throws parse_error on failure.
    inline std::int_least64_t read_int64(void)
    {
        return read_intg<std::int_least64_t, iutil::int64_min, iutil::int64_max>("int64");
    }

    // Read a 32-bit unsigned integer.
    // Throws parse_error on failure.
    inline std::uint_least32_t read_uint32(void)
    {
        return read_uintg<std::uint_least32_t, iutil::uint32_max>("uint32");
    }

    // Read a 64-bit unsigned integer.
    // Throws parse_error on failure.
    inline std::uint_least64_t read_uint64(void)
    {
        return read_uintg<std::uint_least64_t, iutil::uint64_max>("uint64");
    }

    // Read a float.
    // Throws parse_error on failure.
    inline float read_float(const unsigned rdflags = RDFLAG_none)
    {
        return read_floating<float>(rdflags);
    }

    // Read a double.
    // Throws parse_error on failure.
    inline double read_double(const unsigned rdflags = RDFLAG_none)
    {
        return read_floating<double>(rdflags);
    }

    // Read any number (integer or floating-point).
    // Throws parse_error on failure.
    inline number read_number(const unsigned rdflags = RDFLAG_none)
    {
        number value;
        error e = try_read_number(value, rdflags);
        if (e) throw parse_error(m_is.ipos(), e);
        return value;
    }

    // Read a boolean. 
    // Throws parse_error on failure.
    inline bool read_bool(void)
    {
        if (!assumes_no_ws())
            skip_ws();

        int value = do_read_bool(m_is);
        if (value == -1)
            throw parse_error(m_is.ipos(), ERROR_token_bool);

        return value ? true : false;
    }

    // Read null. 
    // Throws parse_error on failure.
    inline void read_null(void)
    {
        error e = try_read_null();
        if (e) throw parse_error(m_is.ipos(), e);
    }

    // Read string. String is unescaped. 
    // Throws parse_error on failure.
    template <typename StrAllocator = std::allocator<CharT>>
    inline std::basic_string<CharT, ChTraits, StrAllocator> read_string(void)
    {
        basic_out_stdstr<CharT, StrAllocator> os;
        if (!assumes_no_ws())
            skip_ws();

        // avoid try_read_string() (skip string initialization)
        error e = read_str(m_is, os);
        if (e) throw parse_error(m_is.ipos(), e);
        return std::move(os).str();
    }

    // Read string into output object. 
    // Throws parse_error on failure.
    //
    // flags may contain one or more of:
    // - RDFLAG_str_copy: copy string as-is (without unescaping).
    // - RDFLAG_str_only: no quote delimiters, read input till end.
    //
    template <typename Output>
    inline void read_string_to(Output& out, const unsigned rdflags = RDFLAG_none)
    {
        error e = try_read_string_to(out, rdflags);
        if (e) throw parse_error(m_is.ipos(), e);
    }

    // Read value of arbitrary type.
    // Throws parse_error on failure.
    // 
    // T may be any integral type, float, double,
    // sijson::number, std::nullptr_t, bool,
    // or a specialization of std::basic_string<>.
    //
    template <typename T>
    inline T read(void)
    {
        return do_read<T>(*this);
    }

    // ----------------------------------  no-throw/no-allocate API ----------------------------------
    // See each function's noexcept() specifier for its nothrow + no-allocate requirements.
    // At minimum, all relevant member fns of the input (peek(), take(), etc.) must be non-throwing.
    // -----------------------------------------------------------------------------------------------

    template <typename IntT>
    inline error try_read_sint(IntT& out_value)
    {

    }

    // Try to read a floating-point value.
    template <typename T>
    inline error try_read_floating(T& out_value, const unsigned rdflags = RDFLAG_none)
        noexcept(IsNoThrowInput)
    {
        if (!assumes_no_ws())
            skip_ws();

        switch (rdflags)
        {
        case RDFLAG_allow_nan_inf:
            return read_fp<true>(m_is, out_value);
        case RDFLAG_none:
            return read_fp<false>(m_is, out_value);
        default:
            return ERROR_bad_rdflags;
        }
    }

    // Try to read a number.
    inline error try_read_number(number& out_num, const unsigned flags = RDFLAG_none)
        noexcept(IsNoThrowInput)
    {
        if (!assumes_no_ws())
            skip_ws();

        switch (flags)
        {
        case RDFLAG_allow_nan_inf:
            return read_num<true>(m_is, out_num);
        case RDFLAG_none:
            return read_num<false>(m_is, out_num);
        default:
            return ERROR_bad_rdflags;
        }
    }

    // Try to read a boolean.
    inline error try_read_bool(bool& out_bool)
        noexcept(IsNoThrowInput)
    {
        if (!assumes_no_ws())
            skip_ws();

        int value = do_read_bool(m_is);
        if (value == -1)
            return ERROR_token_bool;

        out_bool = value ? true : false;
        return ERROR_none;
    }

    // Try to read null.
    inline error try_read_null(void)
        noexcept(IsNoThrowInput)
    {
        if (!assumes_no_ws())
            skip_ws();

        return try_consume_str(m_is, iutil::strings<CharT>::null_str) ?
            ERROR_none : ERROR_token_null;
    }

    // Try to read string. String is unescaped.
    template <typename StrAllocator>
    inline error try_read_string(std::basic_string<CharT, ChTraits, StrAllocator>& out_str)
    {
        basic_out_stdstr<CharT, StrAllocator> os;
        if (!assumes_no_ws())
            skip_ws();

        error e = read_str(m_is, os);
        if (!e) out_str = std::move(os).str();
        return e;
    }

    // Try to read string into output object.
    //
    // flags may contain one or more of:
    // - RDFLAG_str_copy: copy string as-is (without unescaping).
    // - RDFLAG_str_only: no quote delimiters, read input till end.
    //
    template <typename Output>
    inline error try_read_string_to(Output& out, const unsigned rdflags = RDFLAG_none)
        noexcept(IsNoThrowInput && IsNoThrowOutput<Output>::value)
    {
        static_assert(
            std::is_same<typename to_si_output_t<Output>::char_type, CharT>::value,
            "Output must have same char_type as input.");

        wrap_output_t<Output> os(out);

        if (!assumes_no_ws())
            skip_ws();

        switch (rdflags)
        {
        case RDFLAG_none:
            return read_str(m_is, os);

        case RDFLAG_str_copy:
            return copy_str(m_is, os);

        case RDFLAG_str_only:
            return read_str_noquotes(m_is, os);

        case RDFLAG_str_copy | RDFLAG_str_only:
            return copy_str_noquotes(m_is, os);

        default: return ERROR_bad_rdflags;
        }
    }

    // Get input object.
    inline input_type& in(void) noexcept { return m_is; }


private:
    template <typename UintT>
    static inline bool do_read_uintg(MyInput& is, UintT& out_value)
    {
        if (is.end() || !iutil::is_digit(is.peek()) || is.peek() == CharT('-'))
            return false;

        out_value = 0;
        while (!is.end() && iutil::is_digit(is.peek()))
        {
            UintT old = out_value;
            CharT digit = is.peek() - CharT('0');
            out_value = 10 * out_value + UintT(digit);
            if (out_value < old) return false; // overflow
            is.take();
        }
        return true;
    }

    template <typename IntT, IntT Lbound, IntT Ubound>
    static inline bool do_read_intg(MyInput& is, IntT& out_value)
    {
        bool neg = is.peek() == CharT('-');
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

    // Read all chars matching a JSON number pattern.
    // This is slow! Use fastfloat instead whenever possible.
    // Does not check if number is within range!
    template <bool AllowInfNan = false, bool Validate = true, typename Output>
    static SIJSON_ALWAYS_INLINE error read_numstr(MyInput& is, Output& buf) 
    {
        using OutCharT = typename Output::char_type;

        if (is.end())
            return ERROR_invalid_num;

        if (AllowInfNan)
            throw parse_error(is.ipos(), "Unimplemented");

        const bool neg = is.peek() == CharT('-');
        if (neg) buf.put(OutCharT(is.take()));

        // check leading zeros
        if (Validate && !is.end() && is.peek() == CharT('0'))
        {
            buf.put(OutCharT(is.take()));
            if (!is.end() && iutil::is_digit(is.peek()))
                return ERROR_invalid_num;
        }

        while (!is.end() && iutil::is_digit(is.peek()))
            buf.put(OutCharT(is.take()));

        // at least one digit on left
        if (Validate && (neg ? buf.opos() == 1 : buf.opos() == 0))
            return ERROR_invalid_num;

        if (is.peek() == CharT('.'))
        {
            buf.put(OutCharT(is.take()));

            auto old_len = buf.opos();
            while (!is.end() && iutil::is_digit(is.peek()))
                buf.put(OutCharT(is.take()));

            // at least one digit on right
            if (Validate && buf.opos() == old_len)
                return ERROR_invalid_num;
        }

        if (is.peek() == CharT('e') || is.peek() == CharT('E'))
        {
            buf.put(OutCharT(is.take()));

            if (is.peek() == CharT('+') || is.peek() == CharT('-'))
                buf.put(OutCharT(is.take()));

            while (!is.end() && iutil::is_digit(is.peek()))
                buf.put(OutCharT(is.take()));
        }

        return ERROR_none;
    }

    // read_fp() stdlib fallback
#if SIJSON_HAS_STRTOFP_L
    template <bool AllowInfNan, typename T>
    static inline error read_fp_slow(MyInput& is, T& out_val) 
    {
        out_cstrspan buf(iutil::TLB_FP<>::BUF);
        error e = read_numstr<AllowInfNan>(is, buf);
        if (e) return e;

        char* eptr;
        int& Errno = errno;

        Errno = 0;
        out_val = iutil::strtofp<T>(buf.opbeg(), &eptr);
        if (Errno)
        {
            switch (Errno)
            {
            case ERANGE: return ERROR_out_of_range;
            case EINVAL: return ERROR_invalid_num;
            default: return ERROR_invalid_num;
            }
        }

        SIJSON_ASSERT(eptr == buf.opend());
        (void)eptr; // maybe unused
        return ERROR_none;
    }
#else
    template <bool AllowInfNan, typename T>
    static inline error read_fp_slow(MyInput& is, T& out_val) 
    {
        out_strspan buf(iutil::TLB_FP<>::BUF);
        error e = read_numstr<AllowInfNan>(is, buf);
        if (e) return e;

        iutil::memspanbuf sbuf(outdata(buf), std::ios_base::in);
        std::istream sis(&sbuf);
        sis.imbue(std::locale::classic());
        sis >> out_val;

        if (sis.fail())
            return ERROR_invalid_num;

        SIJSON_ASSERT(sbuf.gptr() == sbuf.egptr());
        return ERROR_none;
    }
#endif

#if SIJSON_USE_FASTFLOAT
    template <bool AllowInfNan>
    static constexpr fast_float::parse_options ffopts()
    {
        return fast_float::parse_options(
            fast_float::chars_format::general, fast_float::parse_rules::json_rules, '.', AllowInfNan);
    }

    template <bool AllowInfNan, typename CgInput, typename T>
    static SIJSON_CONSTEXPR20 error ff_from_chars(CgInput& is, T& out_val)
    {
        auto res = fast_float::from_chars_advanced(is.ipcur(), is.ipend(), out_val, ffopts<AllowInfNan>());
        if (res.ec == std::errc())
        {
            is.icommit(res.ptr - is.ipcur());
            return ERROR_none;
        }
        else if (res.ec == std::errc::result_out_of_range)
            return ERROR_out_of_range;
        else
            return ERROR_invalid_num;
    }

    // True if T is supported by fast_float.
    template <typename T>
    static constexpr bool is_ff_type()
    {
        return
            std::is_same<T, float>::value || std::is_same<T, double>::value ||
            (std::is_same<T, long double>::value &&
                std::numeric_limits<double>::digits == std::numeric_limits<long double>::digits);
    }

    template <bool AllowInfNan, typename CgInput, typename T>
    static SIJSON_CONSTEXPR20 error read_fp_fast(CgInput& is, T& out_val)
    {
        static_assert(is_ff_type<T>(), "");

        if (std::is_same<T, float>::value || std::is_same<T, double>::value)
            return ff_from_chars<AllowInfNan>(is, out_val);
        else
        {
            double val;
            error e = ff_from_chars<AllowInfNan>(is, val);
            out_val = val;
            return e;
        }
    }
#endif
    // read_fp_fast() -> input is contiguous and type is supported by fast float
    // read_fp_slow() -> everything else

    template <
        bool AllowInfNan = false, typename IOKind = InputKind, typename T,
        iutil::enable_if_t<std::is_same<IOKind, io_contiguous>::value> = 0
    >
    static inline error read_fp(MyInput& is, T& out_val)
    {
#if SIJSON_USE_FASTFLOAT
        if (is_ff_type<T>())
            return read_fp_fast<AllowInfNan>(is, out_val);
#endif
        return read_fp_slow<AllowInfNan>(is, out_val);
    }

    template <
        bool AllowInfNan = false, typename IOKind = InputKind, typename T,
        iutil::enable_if_t<!std::is_same<IOKind, io_contiguous>::value> = 0
    >
    static inline error read_fp(MyInput& is, T& out_val)
    {
#if SIJSON_USE_FASTFLOAT
        if (is_ff_type<T>())
        {
            out_strspan buf(iutil::TLB_FP<>::BUF);
            error e = read_numstr<AllowInfNan, false>(is, buf);
            if (e) return e;

            in_str ibuf(outdata(buf));
            return read_fp_fast<AllowInfNan>(ibuf, out_val);
        }
#endif
        return read_fp_slow<AllowInfNan>(is, out_val);
    }

    static SIJSON_ALWAYS_INLINE bool set_num_if_int64(
        const fast_float::parsed_number_string<CharT>& ps, number& out_value)
    {
        const auto nintdigits = ps.integer.len();
        
        if (ps.valid && ps.integer.ptr + nintdigits == ps.lastmatch)
        {
            const std::uint64_t int_value = ps.mantissa;

            static constexpr std::uint64_t min_20_digit_uint{ 10000000000000000000ULL };
            static constexpr auto abs_int64_min = iutil::absu(std::numeric_limits<std::int64_t>::min());

            if (ps.negative && (nintdigits < 19 || (nintdigits == 19 && int_value <= abs_int64_min))) {
                out_value = iutil::uneg<std::intmax_t>(int_value);
                return true;
            }
            else if (!ps.negative && (nintdigits < 20 || (nintdigits == 20 && int_value >= min_20_digit_uint))) {
                out_value = int_value;
                return true;
            }
        }
        return false;
    }
    
//    static inline bool scan_is_int(MyInput& is)
//    {
//        static_assert(std::is_same<typename MyInput::input_kind, io_contiguous>::value, "");
//
//        if (inrem(is) >= 20) // max uint64 digits 
//        {
//#if SIJSON_SSE2
//            if (sizeof(CharT) == 1)
//            {
//                
//                __m128i d1 = _mm_loadu_si128(reinterpret_cast<const __m128i*>(is.ipcur());
//                std::uint_least32_t d2;
//                std::memcpy(&d2, is.ipcur(), 4);
//
//            }
//            
//#endif
//        }
//    }

#ifdef SIJSON_USE_FASTFLOAT
    template <bool AllowInfNan, typename CgInput>
    static SIJSON_CONSTEXPR20 error read_num_fast(CgInput& is, number& out_val)
    {
        const CharT* p = is.ipcur();
        const CharT *const pend = is.ipend();

        bool neg = (*p == CharT('-'));
        if (neg) ++p;

        std::uint64_t i = 0;
        const CharT* const pbeg = p;
        while (p != pend && iutil::is_digit(*p))
        {
            i = 10 * i + std::uint64_t(*p - CharT('0'));
            ++p;
        }
        auto nidigits = p - pbeg;

        auto lc = iutil::to_lower(*p);
        if ((*p == CharT('.') || *p == CharT('e') || *p == CharT('E')) ||
            // possible inf/nan?
            (AllowInfNan && nidigits == 0 && (lc == CharT('i') || lc == CharT('n'))) ||
            // too small for int64?
            (neg && (nidigits > 19 || i > 9223372036854775808ULL)) ||
            // too large for uint64?
            (!neg && (nidigits > 20 || (nidigits == 20 && i < 10000000000000000000ULL))))
        {
            // start again, this is floating-point.
            // unfortunately this re-parses integer part, for now
            double val;
            error e = read_fp_fast<AllowInfNan>(is, val);
            out_val = val;
            return e;
        }      
        if (nidigits == 0)
            return ERROR_invalid_num;
    
        out_val = neg ? iutil::uneg<std::int64_t>(i) : i;
        is.icommit(nidigits + neg);
        return ERROR_none;
    }
#endif

    template <bool AllowInfNan>
    static SIJSON_CONSTEXPR20 error read_num_slow(MyInput& is, number& out_val)
    {
        
    }

    template <
        bool AllowInfNan = false, typename IOKind = InputKind,
        iutil::enable_if_t<std::is_same<IOKind, io_contiguous>::value> = 0
    >
    static inline error read_num(MyInput& is, number& out_val)
    {

        constexpr auto ff_options = ffopts<AllowInfNan>();

        fast_float::parsed_number_string<CharT> ps =
            fast_float::parse_number_string(is.ipcur(), is.ipend(), ff_options);

        if (set_num_if_int64(ps, out_val))
        {
            is.icommit(ps.lastmatch - is.ipcur());
            return ERROR_none;
        }

        double value;
        auto res = fast_float::from_chars_preparsed(ps, is.ipcur(), is.ipend(), value, ff_options);
        if (res.ec == std::errc())
        {
            is.icommit(res.ptr - is.ipcur());
            out_val = value;
            return ERROR_none;
        }
        else if (res.ec == std::errc::result_out_of_range)
            return ERROR_out_of_range;
        else
            return ERROR_invalid_num;
    }

    template <
        bool AllowInfNan = false, typename IOKind = InputKind,
        iutil::enable_if_t<!std::is_same<IOKind, io_contiguous>::value> = 0
    >
    static inline error read_num(MyInput& is, number& out_value)
    {
        out_str buf(iutil::max_outchars10<double>::value);
        // todo: this is slow. ideally we should parse only once
        error e = read_numstr(is, buf);
        if (e) return e;

        in_str ibuf(outdata(buf));
        return read_num<AllowInfNan, io_contiguous>(ibuf, out_value);
    }

    template <std::size_t N, typename IOKind>
    static inline bool do_try_consume_str(MyInput& is, const CharT(&str)[N], IOKind)
    {
        static_assert(N > 0, "");

        for (std::size_t i = 0; i < N; ++i)
            if (!iutil::take(is, str[i]))
                return false;
        return true;
    }

    template <std::size_t N>
    static inline bool do_try_consume_str(MyInput& is, const CharT(&str)[N], io_contiguous)
    {
        static_assert(N > 0, "");

        if (inrem(is) >= N)
        {
            auto* src = is.ipcur();
            for (std::size_t i = 0; i < N; ++i)
                if (src[i] != str[i])
                    return false;

            is.icommit(static_cast<InputSize>(N));
            return true;
        }
        else return false;
    }

    template <std::size_t N>
    static inline bool try_consume_str(MyInput& is, const CharT(&str)[N])
    {
        return do_try_consume_str(is, str, InputKind{});
    }

    // calling this instead of try_read_bool() generates better assembly
    static inline int do_read_bool(MyInput& is) noexcept(IsNoThrowInput)
    {
        // check true first! (if inrem < 4, compiler can skip checking false)
        return
            try_consume_str(is, iutil::strings<CharT>::true_str) ? 1 :
            try_consume_str(is, iutil::strings<CharT>::false_str) ? 0 : -1;
    }

    template <typename SiOutput>
    static SIJSON_ALWAYS_INLINE bool unescape(MyInput& is, SiOutput& os)
    {
        is.take(); // skip '\'
        if (is.end())
            return false;

        auto c = is.peek();
        auto uc = iutil::ctrl_lut(c);
        if (uc != 0)
        {
            os.put(uc);
            is.take();
        }
        else if (c == CharT('u'))
        {
            is.take();
            if (!iutil::put_utf_unescape(is, os, InputKind{}))
                return false;
        }
        else return false;
        return true;
    }

    //template <typename SiOutput>
    //static inline bool copy_escaped(MyInput& is, SiOutput& os)
    //{
    //    os.put(is.take()); // copy '\'
    //    if (is.end())
    //        return false;
    //
    //    auto c = is.peek();
    //    if (c <= static_cast<CharT>(127) &&
    //        iutil::unesc_ctrl_lut[ChTraits::to_int_type(c)] != 0)
    //    {
    //        os.put(is.take());
    //    }
    //    else if (c == 0x75) // 'u' (unicode)
    //    {
    //        os.put(is.take());
    //        if (!iutil::copy_escaped_utf(is, os, InputKind{}))
    //            return false;
    //    }
    //    else return false;
    //    return true;
    //}

    template <typename SiOutput>
    static inline error read_str(MyInput& is, SiOutput& os)
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
    static inline error copy_str(MyInput& is, SiOutput& os)
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
                os.put(is.take()); // '\'
                if (is.end())
                    return ERROR_str_escape;

                if (is.peek() == 0x75) // 'u' (unicode)
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

    template <typename SiOutput, typename IOKind>
    static inline void copy_unesc_str_noquotes(MyInput&, SiOutput&, IOKind) {}

    template <typename SiOutput>
    static inline void copy_unesc_str_noquotes(MyInput& is, SiOutput& os, io_contiguous)
    {
        auto n_in = sijson::inrem(is);
        auto* escp = ChTraits::find(is.ipcur(), n_in, 0x5c); // '\'

        InputSize n_out = escp ? static_cast<InputSize>(escp - is.ipcur()) : n_in;
        os.put_n(is.ipcur(), static_cast<std::size_t>(n_out));
        is.icommit(n_out);
    }

    static constexpr bool did_copy_unesc_str_noquotes()
    {
        return std::is_same<InputKind, io_contiguous>::value;
    }

    template <typename SiOutput>
    static inline error read_str_noquotes(MyInput& is, SiOutput& os)
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
    static inline error do_copy_str_noquotes(MyInput& is, SiOutput& os, io_contiguous)
    {
        auto n = (std::size_t)(is.ipend() - is.ipcur());
        os.put_n(is.ipcur(), n);
        is.icommit(n);
        return ERROR_none;
    }

    template <typename SiOutput>
    static inline error do_copy_str_noquotes(MyInput& is, SiOutput& os, io_basic)
    {
        while (!is.end())
            os.put(is.take());
        return ERROR_none;
    }

    template <typename SiOutput>
    static inline error do_copy_str_noquotes(MyInput& is, SiOutput& os, io_buffered)
    {
        return do_copy_str_noquotes(is, os, io_basic{});
    }

    template <typename SiOutput>
    static inline error copy_str_noquotes(MyInput& is, SiOutput& os)
    {
        return do_copy_str_noquotes(is, os, InputKind{});
    }

    template <typename IntT,
        IntT Lbound = std::numeric_limits<IntT>::min(),
        IntT Ubound = std::numeric_limits<IntT>::max()>
    inline IntT read_intg(const char* type_name = nullptr)
    {
        if (!assumes_no_ws()) skip_ws();
        if (m_is.end()) goto fail;

        IntT value;
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
        if (!assumes_no_ws()) skip_ws();
        if (m_is.end()) goto fail;

        UintT value;
        if (!do_read_uintg(m_is, value)) goto fail;
        if (value > Ubound) goto fail;
        return value;
    fail:
        throw parse_error(m_is.ipos(),
            type_name ? type_name : "expected unsigned integral");
    }

    template <typename T>
    SIJSON_ALWAYS_INLINE T read_floating(const unsigned rdflags = RDFLAG_none)
    {
        T value;
        error e = try_read_floating(value, rdflags);
        if (e) throw parse_error(m_is.ipos(), e);
        return value;
    }

    inline void read_char(CharT c, error e)
    {
        if (!skip_ws() || !iutil::take(m_is, c))
            throw parse_error(m_is.ipos(), e);
    }

    template <typename T, iutil::enable_if_t<iutil::is_nb_signed_integral<T>::value> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { return r.read_intg<T>(); }

    template <typename T, iutil::enable_if_t<iutil::is_nb_unsigned_integral<T>::value> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { return r.read_uintg<T>(); }

    template <typename T, iutil::enable_if_t<std::is_floating_point<T>::value> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { return r.read_floating<T>(); }

    template <typename T, iutil::enable_if_same_t<T, number> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { return r.read_number(); }

    template <typename T, iutil::enable_if_same_t<T, bool> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { return r.read_bool(); }

    template <typename T, iutil::enable_if_t<iutil::is_instance_of_basic_string<T, CharT, ChTraits>::value> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { return r.read_string<typename T::allocator_type>(); }

    template <typename T, iutil::enable_if_same_t<T, std::nullptr_t> = 0>
    static SIJSON_ALWAYS_INLINE T do_read(raw_reader& r) { r.read_null(); return nullptr; }

private:
    wrap_input_t<Input> m_is;
};
}

#endif