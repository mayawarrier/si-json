
#ifndef SIJSON_CONFIG
#define SIJSON_CONFIG

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


#include <string>
#if defined(__cpp_lib_string_view) || SIJSON_CPLUSPLUS >= 201703L
#define SIJSON_HAS_STRING_VIEW 1
#else
#define SIJSON_HAS_STRING_VIEW 0
#endif


// Is std::launder required to access an object in byte storage 
// whose size and alignment is exactly equal to the object stored?
// https://en.cppreference.com/w/cpp/types/aligned_storage
// https://stackoverflow.com/a/70419156
// This is false for most or all compilers and is disabled by default.
// Enabling it may incur a performance penalty in GCC.
//
#ifndef SIJSON_USE_LAUNDER_TO_ACCESS_ALIGNED_BYTE_STORAGE
#define SIJSON_USE_LAUNDER_TO_ACCESS_ALIGNED_BYTE_STORAGE 0
#endif

#endif
