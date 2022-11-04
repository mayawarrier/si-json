
#ifndef SIJSON_CONFIG
#define SIJSON_CONFIG

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

#if SIJSON_HAS_INCLUDE(<optional>) && SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_OPTIONAL 1
#else
#define SIJSON_HAS_OPTIONAL 0
#endif

#if defined(__cpp_lib_byte) || SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STDBYTE 1
#else
#define SIJSON_HAS_STDBYTE 0
#endif


#if SIJSON_HAS_ATTRIBUTE(always_inline)
#define SIJSON_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define SIJSON_ALWAYS_INLINE __forceinline
#else
#define SIJSON_ALWAYS_INLINE
#endif


// Is std::launder required to access an object in properly aligned 
// byte storage whose size is at least that of the object stored?
// https://en.cppreference.com/w/cpp/types/aligned_storage
// https://stackoverflow.com/a/70419156
// This is false for most or all compilers so is disabled by default.
// Enabling it may incur a performance penalty in GCC.
//
#ifndef SIJSON_USE_LAUNDER_FOR_ALIGNED_BYTE_STORAGE
#define SIJSON_USE_LAUNDER_FOR_ALIGNED_BYTE_STORAGE 0
#endif


// If enabled, logic_errors will be preferred over assert()s where approppriate.
// This can result in signficant runtime overhead so is disabled by default.
//
#ifndef SIJSON_PREFER_LOGIC_ERRORS
#define SIJSON_PREFER_LOGIC_ERRORS 0
#endif

#endif
