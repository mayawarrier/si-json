# si-json

Was an experimental header-only JSON library for C++ (abandoned and may be currently broken).

The aim was to try to offer a fully std-library-compliant interface i.e.
- Support for custom allocators.
- Complete support for user-defined fancy pointers.
- Constexpr everything.
- Backwards-compatible upto C++11.

as well as be or support the following things:
- Support for Unicode (UTF8, 16, 32, LE or BE)
- Support for every platform supported by libc++.
- Extensible interface (can implement your own input/output streams, readers/writers etc)
- As fast as possible in the usual/common case.


Turns out this is a ridiculously difficult mandate for one person! (who knew?)

I worked on this for 2 years in my free time. The features above are in various states of completion.
