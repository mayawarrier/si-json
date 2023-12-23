// Config macros and platform support.

#ifndef SIJSON_CONFIG_HPP
#define SIJSON_CONFIG_HPP

// Default file buffer size.
#ifndef SIJSON_FILEBUF_SIZE
#define SIJSON_FILEBUF_SIZE 4096
#endif

// Accessing an object in aligned storage without std::launder is
// _technically_ UB since P0137R1 (C++17), but no compilers seem to 
// care as of 2023.
// https://en.cppreference.com/w/cpp/types/aligned_storage
// https://stackoverflow.com/a/70419156
// If enabled, some compilers might generate worse assembly.
//
#ifndef SIJSON_LAUNDER_ALIGNED_STORAGE
#define SIJSON_LAUNDER_ALIGNED_STORAGE 0
#endif

// launder required only after P0137R1
#if SIJSON_LAUNDER_ALIGNED_STORAGE && SIJSON_CPLUSPLUS < 201606L
#undef SIJSON_LAUNDER_ALIGNED_STORAGE
#define SIJSON_LAUNDER_ALIGNED_STORAGE 0
#endif

// If enabled, std::logic_errors will be used in place of 
// assert()s or std::unreachable() where appropriate.
// This may cause significant runtime overhead.
//
#ifndef SIJSON_USE_LOGIC_ERRORS
#define SIJSON_USE_LOGIC_ERRORS 0
#endif

// If enabled, stack-traces will be included in exception
// messages. This may cause significant runtime overhead.
//
#ifndef SIJSON_EXC_STACKTRACE
#define SIJSON_EXC_STACKTRACE 0
#endif

//// Strengthen library i.e. always assert
////
//#ifndef SIJSON_CHECKED
//#define SIJSON_CHECKED 0
//#endif
//
//#if SIJSON_CHECKED
//// applies even in release mode
//#define SIJSON_USE_ASSERTS 1
//#define SIJSON_USE_LOGIC_ERRORS 1
//#define SIJSON_CSAFE(t) t##_s
//#else
//#define SIJSON_CSAFE(t) t
//#endif


#include <cstddef>
#include <cassert>
#include <cfloat>

#ifdef _MSC_VER
#define SIJSON_CPLUSPLUS _MSVC_LANG
#else
#define SIJSON_CPLUSPLUS __cplusplus
#endif

#define SIJSON_STRFY(...) #__VA_ARGS__
#define SIJSON_XSTRFY(x) SIJSON_STRFY(x)

#define SIJSON_SRCLOC __FILE__ ":" SIJSON_XSTRFY(__LINE__)

#ifdef __has_builtin
#define SIJSON_HAS_BUILTIN(x) __has_builtin(x)
#else
#define SIJSON_HAS_BUILTIN(x) 0
#endif

#ifdef __has_include
#define SIJSON_HAS_INCLUDE(x) __has_include(x)
#else
#define SIJSON_HAS_INCLUDE(x) 0
#endif

#ifdef __has_attribute
#define SIJSON_HAS_ATTRIBUTE(x) __has_attribute(x)
#else
#define SIJSON_HAS_ATTRIBUTE(x) 0
#endif

#if SIJSON_HAS_INCLUDE(<version>)
#include <version>
#endif

#if SIJSON_HAS_INCLUDE(<string_view>) && SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STRING_VIEW 1
#endif

#if defined(__cpp_lib_byte) || SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STDBYTE 1
#endif

// gcc is non-conforming: has_include succeeds but header is
// unusable unless __cpp_lib_stacktrace is defined
#if SIJSON_HAS_INCLUDE(<stacktrace>) && __cpp_lib_stacktrace >= 202011L
#define SIJSON_HAS_STACKTRACE 1
#endif

// xlocale.h removed from GCC 2.26 onwards
// https://github.com/tpaviot/oce/issues/689
// newlib (cygwin) has strtof/ld/l_l from v2.4 onwards
// https://github.com/bminor/newlib/commit/238455adfab4f8070ac65400aac22bb8a9e502fc
#if defined(_MSC_VER) || \
    defined(__wasi__) || \
    (defined(__NEWLIB_VERSION__) && __NEWLIB__ >= 2 && __NEWLIB_MINOR__ >= 4 && defined(__GNU_VISIBLE)) || \
    (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 25))
#define SIJSON_HAS_STRTOFP_L 1

#elif defined(__GLIBC__) || defined(__BIONIC__) || defined(__APPLE__) || defined(__FreeBSD__)
#include <xlocale.h>
#define SIJSON_HAS_STRTOFP_L 1
#endif

#ifdef _MSC_VER
#define SIJSON_ALWAYS_INLINE __forceinline
#elif SIJSON_HAS_ATTRIBUTE(always_inline)
#define SIJSON_ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define SIJSON_ALWAYS_INLINE
#pragma warning "SIJSON_ALWAYS_INLINE not defined"
#endif

#ifdef _MSC_VER
#define SIJSON_NEVER_INLINE __declspec(noinline)
#elif SIJSON_HAS_ATTRIBUTE(noinline)
#define SIJSON_NEVER_INLINE __attribute__((noinline))
#else
#define SIJSON_NEVER_INLINE
#pragma warning "SIJSON_NEVER_INLINE not defined"
#endif

#if SIJSON_CPLUSPLUS >= 202002L
#define SIJSON_CONSTEXPR20 constexpr
#else
#define SIJSON_CONSTEXPR20 inline
#endif

// todo: more compilers that don't support magic statics
#if _MSC_VER < 1900
#define SIJSON_NO_MAGIC_STATICS 1
#endif

#if FLT_MANT_DIG == 24 && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128 && \
    DBL_MANT_DIG == 53 && DBL_MIN_EXP == -1021 && DBL_MAX_EXP == 1024
#define SIJSON_USE_FASTFLOAT 1
#else
#define SIJSON_USE_FASTFLOAT 0
#endif


// MSVC prefixes POSIX functions with an underscore
#ifdef _MSC_VER
#define SIJSON_MSVC_OR_POSIX(x) _##x
#else
#define SIJSON_MSVC_OR_POSIX(x) x
#endif

#if defined(__GNUC__)
// disable -Wcast-align=strict (GCC only)
#define SIJSON_IGNORE_WCAST_ALIGN \
  _Pragma("GCC diagnostic push") \
  _Pragma("GCC diagnostic ignored \"-Wcast-align\"")
#else
#define SIJSON_IGNORE_WCAST_ALIGN
#endif

#if defined(__GNUC__)
#define SIJSON_RESTORE_WCAST_ALIGN \
  _Pragma("GCC diagnostic pop")
#else
#define SIJSON_RESTORE_WCAST_ALIGN
#endif


// Enable runtime asserts in library code.
// 
// By default this is tied to NDEBUG, but you can change
// this by defining the macro yourself
// (if you want asserts in a Release build, for example)
// 
// You can also provide a custom assert by defining SIJSON_ASSERT().
// 
#ifndef SIJSON_USE_ASSERTS
#ifdef NDEBUG
#define SIJSON_USE_ASSERTS 0
#else
#define SIJSON_USE_ASSERTS 1
#endif
#endif

#if !defined(SIJSON_ASSERT) && SIJSON_USE_ASSERTS
#ifdef NDEBUG
#include <cstdio>
#include <exception>

namespace sijson {
namespace internal {
// schrodinger's inline function!
SIJSON_NEVER_INLINE inline 
void assert_fail(const char* src_loc, const char* msg)
{
    std::fprintf(stderr, "\n%s: Assertion '%s' failed.", src_loc, msg);
    std::terminate();
}
}}
#define SIJSON_ASSERT(cond) \
(void)( \
    (!!(cond)) || \
    (::sijson::internal::assert_fail(SIJSON_SRCLOC, #cond), 0) \
)
#else
#define SIJSON_ASSERT(cond) assert(cond)
#endif
#else
#define SIJSON_ASSERT(cond)
#endif





// Allow the use of SIMD instructions to improve performance.
// Define the SIMD instruction set(s) available.
//
#if SIJSON_SSE2 || SIJSON_SSE42 || SIJSON_AVX2
#define SIJSON_SIMD 1
#endif

#if SIJSON_AVX2 && !(SIJSON_SSE2 || SIJSON_SSE42)
#define SIJSON_SSE2 1
#define SIJSON_SSE42
#endif

#if SIJSON_SSE2 && SIJSON_USE_FASTFLOAT
#define FASTFLOAT_SSE2 1
#endif




#endif
