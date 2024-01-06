# si-json

Was an experimental header-only JSON library for C++ (abandoned and may be currently broken).

The aim was to offer a fully std-library compliant interface i.e.
- Support for custom allocators and user-defined fancy pointers (like those from Boost.Interprocess).
- Constexpr everything.
- Backwards-compatible upto C++11.

as well as be or support the following things:
- Support for Unicode (UTF8, 16, 32, LE or BE)
- Support for every platform supported by libc++.
- Extensible interface (can implement your own input/output streams, readers/writers etc)
- As fast as possible in the usual/common case.


I worked on this for 2 years in my free time. The features above are in various states of completion.

[`internal/core.hpp`](https://github.com/mayawarrier/si-json/blob/main/include/sijson/internal/core.hpp) contains many useful utilities to implement std library types, including an SSO string type. \
It also contains all core code required to properly support custom allocators and fancy pointers. Most objects in sijson fully support both.
