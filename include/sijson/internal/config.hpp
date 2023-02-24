/*
* Modify build configuration here.
*/

#ifndef SIJSON_CONFIG_HPP
#define SIJSON_CONFIG_HPP

#include <cstddef>

#ifdef _MSC_VER
#define SIJSON_CPLUSPLUS _MSVC_LANG
#else
#define SIJSON_CPLUSPLUS __cplusplus
#endif

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

#if SIJSON_HAS_INCLUDE(<cxxabi.h>)
#define SIJSON_HAS_CXXABI_H 1
#else
#define SIJSON_HAS_CXXABI_H 0
#endif

#if SIJSON_HAS_INCLUDE(<string_view>) && SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STRING_VIEW 1
#else
#define SIJSON_HAS_STRING_VIEW 0
#endif

#if defined(__cpp_lib_byte) || SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STDBYTE 1
#else
#define SIJSON_HAS_STDBYTE 0
#endif

#ifdef _MSC_VER
#define SIJSON_ALWAYS_INLINE __forceinline
#elif SIJSON_HAS_ATTRIBUTE(always_inline)
#define SIJSON_ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define SIJSON_ALWAYS_INLINE
#endif


// Change the default file buffer size.
#ifndef SIJSON_FILEBUF_SIZE
#define SIJSON_FILEBUF_SIZE (4096)
#endif


// Allow the use of SIMD instructions to improve performance.
// Define any ONE of the below to specify the SIMD instruction set available.
//
#if defined(SIJSON_SSE2) || defined(SIJSON_AVX2)
#define SIJSON_SIMD
#endif


// Turn on/off asserts in internal library code.
// This will apply even if NDEBUG is defined.
//
#ifndef SIJSON_DEBUG
#define SIJSON_DEBUG 0
#endif


// Is std::launder required to access an object in properly aligned 
// byte storage whose size is at least that of the object stored?
// https://en.cppreference.com/w/cpp/types/aligned_storage
// https://stackoverflow.com/a/70419156
// This is false for most or all compilers so is disabled by default.
//
#ifndef SIJSON_LAUNDER_ALIGNED_STORAGE
#define SIJSON_LAUNDER_ALIGNED_STORAGE 0
#endif


// If enabled, std::logic_errors will be used in place of 
// assert()s or std::unreachable() where appropriate.
// This may cause significant runtime overhead.
//
#ifndef SIJSON_LOGIC_ERRORS
#define SIJSON_LOGIC_ERRORS 0
#endif


#endif
