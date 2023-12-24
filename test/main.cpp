
#include <string>
#include <vector>
#include <iostream>
#include <cassert>
#include <chrono>
#include <numeric>
#include <iomanip>

#include <sstream>
#include <fstream>

#include "sijson/io_wrappers.hpp"
#include "sijson/io_mem.hpp"
#include "sijson/io_string.hpp"
#include "sijson/io_file.hpp"
#include "sijson/utility.hpp"
#include "sijson/reader.hpp"


template<typename T>
class FancyPtr {
    T* ptr = nullptr;
    FancyPtr(T *ptr, bool) : ptr(ptr) {}
public:
    using element_type = T;

    using difference_type = std::ptrdiff_t;
    using value_type = element_type;
    using pointer = element_type * ;
    using reference = element_type & ;
    using iterator_category = std::random_access_iterator_tag;

    FancyPtr() = default;
    FancyPtr(const FancyPtr&) = default;

    FancyPtr& operator=(const FancyPtr&) = default;

    static FancyPtr pointer_to(element_type& r) noexcept {
        return FancyPtr(std::addressof(r), true);
    }

    // allocator::pointer is convertible to allocator::const_pointer
    template<typename U = T, typename std::enable_if<std::is_const<U>::value, int>::type = 0>
    FancyPtr(const FancyPtr<typename std::remove_const<T>::type>& p) : ptr(p.operator->()  /* std::to_address(p) in C++20 */) {}

    // NullablePointer
    FancyPtr(std::nullptr_t) : FancyPtr() {}
    FancyPtr& operator=(std::nullptr_t) {
        ptr = nullptr;
        return *this;
    }
    explicit operator bool() const { return *this != nullptr; }

    // to_address, InputIterator
    pointer operator->() const {
        return ptr;
    }

    // Iterator
    element_type& operator*() const {
        return *ptr;
    }
    FancyPtr& operator++() {
        ++ptr;
        return *this;
    }
    // InputIterator
    friend bool operator==(FancyPtr l, FancyPtr r) {
        return l.ptr == r.ptr;
    }
    friend bool operator!=(FancyPtr l, FancyPtr r) {
        return !(l == r);
    }
    FancyPtr operator++(int) {
        return FancyPtr(ptr++, true);
    }
    // BidirectionalIterator
    FancyPtr& operator--() {
        --ptr;
        return *this;
    }
    FancyPtr operator--(int) {
        return FancyPtr(ptr--, true);
    }
    // RandomAccessIterator
    FancyPtr& operator+=(difference_type n) {
        ptr += n;
        return *this;
    }
    friend FancyPtr operator+(FancyPtr p, difference_type n) {
        return p += n;
    }
    friend FancyPtr operator+(difference_type n, FancyPtr p) {
        return p += n;
    }
    FancyPtr& operator-=(difference_type n) {
        ptr -= n;
        return *this;
    }
    friend FancyPtr operator-(FancyPtr p, difference_type n) {
        return p -= n;
    }
    friend difference_type operator-(FancyPtr a, FancyPtr b) {
        return a.ptr - b.ptr;
    }
    reference operator[](difference_type n) const {
        return ptr[n];
    }
    friend bool operator<(FancyPtr a, FancyPtr b) {
        return std::less<pointer>(a.ptr, b.ptr);
    }
    friend bool operator> (FancyPtr a, FancyPtr b) { return b < a; }
    friend bool operator>=(FancyPtr a, FancyPtr b) { return !(a < b); }
    friend bool operator<=(FancyPtr a, FancyPtr b) { return !(b < a); }


#if defined(_LIBCPP_MEMORY)
    // Extra libc++ requirement (Since libc++ does `static_cast<FancyPtr<U>>(FancyPtr<T>())` sometimes)
    template<typename U> FancyPtr(FancyPtr<U> p) : ptr(static_cast<T*>(p.operator->())) {}
#elif defined(_GLIBCXX_MEMORY)
    // Extra libstdc++ requirement (Since libstdc++ uses raw pointers internally and tries to implicitly cast back
    // and also casts from pointers to different types)
    template<typename U> FancyPtr(FancyPtr<U> p) : ptr(static_cast<T*>(p.operator->())) {}
    FancyPtr(T *ptr) : FancyPtr(ptr, true) {}
    operator T*() { return ptr; }
#endif
};

// NullablePointer (Not strictly necessary because of implicit conversion from nullptr to FancyPtr)
template<typename T>
bool operator==(FancyPtr<T> p, std::nullptr_t) {
    return p == FancyPtr<T>();
}
template<typename T>
bool operator==(std::nullptr_t, FancyPtr<T> p) {
    return FancyPtr<T>() == p;
}
template<typename T>
bool operator!=(FancyPtr<T> p, std::nullptr_t) {
    return p != FancyPtr<T>();
}
template<typename T>
bool operator!=(std::nullptr_t, FancyPtr<T> p) {
    return FancyPtr<T>() != p;
}

template<>
class FancyPtr<void> {
    void* ptr = nullptr;
    FancyPtr(void *ptr, bool) : ptr(ptr) {}
public:
    using element_type = void;
    using pointer = void*;

    FancyPtr() = default;
    FancyPtr(const FancyPtr&) = default;
    template<typename T, typename std::enable_if<!std::is_const<T>::value, int>::type = 0>
    FancyPtr(FancyPtr<T> p) : ptr(static_cast<void*>(p.operator->())) {}

    FancyPtr& operator=(const FancyPtr&) = default;
    FancyPtr& operator=(std::nullptr_t) { ptr = nullptr; return *this; }

    pointer operator->() const {
        return ptr;
    }

    // static_cast<A::pointer>(vp) == p
    template<typename T>
    explicit operator FancyPtr<T>() {
        if (ptr == nullptr) return nullptr;
        return std::pointer_traits<FancyPtr<T>>::pointer_to(*static_cast<T*>(ptr));
    }
};

template<>
class FancyPtr<const void> {
    const void* ptr = nullptr;
    FancyPtr(const void *ptr, bool) : ptr(ptr) {}
public:
    using element_type = const void;
    using pointer = const void*;

    FancyPtr() = default;
    FancyPtr(const FancyPtr&) = default;
    template<typename T>
    FancyPtr(FancyPtr<T> p) : ptr(static_cast<const void*>(p.operator->())) {}

    FancyPtr& operator=(const FancyPtr&) = default;
    FancyPtr& operator=(std::nullptr_t) { ptr = nullptr; return *this; }

    pointer operator->() const {
        return ptr;
    }

    // static_cast<A::const_pointer>(cvp) == cp
    template<typename T>
    explicit operator FancyPtr<const T>() {
        if (ptr == nullptr) return nullptr;
        return std::pointer_traits<FancyPtr<const T>>::pointer_to(*static_cast<const T*>(ptr));
    }
};

template<typename T>
class TrivialAllocator {
public:
    using pointer = FancyPtr<T>;
    using value_type = T;

    TrivialAllocator() = default;

    template<typename Other>
    TrivialAllocator(const TrivialAllocator<Other> &) {}

    TrivialAllocator(const TrivialAllocator &alloc) = default;

    pointer allocate(size_t n) {
        std::allocator<T> alloc;
        return pointer::pointer_to(*std::allocator_traits<std::allocator<T>>::allocate(alloc, n));
    }
    void deallocate(pointer ptr, size_t n) {
        std::allocator<T> alloc;
        // std::to_address(ptr) instead of ptr.operator-> in C++20
        std::allocator_traits<std::allocator<T>>::deallocate(alloc, ptr.operator->(), n);
    }

    bool operator==(const TrivialAllocator &) const { return true; }

    bool operator!=(const TrivialAllocator &) const { return false; }
};



//sijson::out_str gen_random_utf8(unsigned seed)
//{
//    sijson::out_str out;
//    
//    std::srand(seed);
//    for (int i = 0; i < 1114112; ++i)
//    {
//        int r = std::rand() % 0x10FFFF;
//        while (r >= 0xd800 && r <= 0xdfff)
//            r = std::rand() % 0x10FFFF;
//
//        sijson::utf8::put_codepoint(out, (sijson::utf_codepoint_t)r);
//    }
//
//    return out;
//}

#define TEST_UTF8 0
#define TEST_RW 1

SIJSON_CONSTEXPR20 double get_value(void)
{
    double d;
    constexpr char16_t str[] = u"1.0000000000000002220446049250313";
    fast_float::from_chars(str, std::end(str), d);
    return d;
}

int main(int argc, char** argv)
{
    (void)argc;
    (void)argv;

    std::cout.sync_with_stdio(false);

    sijson::iutil::strbuffer<char, std::char_traits<char>, TrivialAllocator<char>> buf;
    //buf.reserve(100);

    sijson::iutil::buffer<char, TrivialAllocator<char>> buf2;
    //buf2.reserve(100);

    // Did EBCO work for an empty allocator?
    static_assert(std::is_standard_layout<sijson::iutil::buffer<char>>::value, "");
    static_assert(std::is_standard_layout<sijson::iutil::strbuffer<char>>::value, "");

    std::cout << "\nSizeof std::size_t: " << sizeof(std::size_t);
    std::cout << "\nSizeof iutil::buffer with trivial custom allocator: " << sizeof(buf2);
    std::cout << "\nSizeof iutil::strbuffer with trivial custom allocator: " << sizeof(buf);
    std::cout << "\nSize of short string buffer: " << buf.min_capacity();

    
    static_assert(sijson::is_contiguous_input<sijson::in_mem, unsigned char>::value, "");
    static_assert(sijson::is_contiguous_input<sijson::in_str, char>::value, "");

    static_assert(sijson::is_contiguous_output<sijson::out_mem, unsigned char>::value, "");
    static_assert(sijson::is_contiguous_output<sijson::out_str, char>::value, "");
    static_assert(sijson::is_contiguous_output<sijson::out_strspan, char>::value, "");
    //static_assert(sijson::is_contiguous_output<sijson::out_stdstr, char>::value, "");

    static_assert(!sijson::is_nothrow_input<std::string, char>::value, "");
    static_assert(!sijson::is_nothrow_basic_input<std::string, char>::value, "");
    static_assert(!sijson::is_nothrow_contiguous_input<std::string, char>::value, "");

    static_assert(sijson::is_nothrow_contiguous_input<sijson::in_mem, unsigned char>::value, "");
    static_assert(sijson::is_nothrow_contiguous_input<sijson::in_str, char>::value, "");
    static_assert(sijson::is_nothrow_contiguous_output<sijson::out_strspan, char>::value, "");
    static_assert(!sijson::is_nothrow_output<sijson::out_strspan_s, char>::value, "");


    static_assert(noexcept(std::declval<sijson::raw_reader<sijson::in_str>>().skip_ws()), "");
    static_assert(noexcept(std::declval<sijson::raw_reader<sijson::in_str>>().try_read_string_to(std::declval<sijson::out_strspan&>())), "");
    static_assert(!noexcept(std::declval<sijson::raw_reader<sijson::in_str>>().try_read_string_to(std::declval<sijson::out_strspan_s&>())), "");

    static_assert(std::is_same<
        sijson::options<sijson::null_terminate, sijson::throw_on_overflow>, 
        sijson::options<sijson::throw_on_overflow, sijson::null_terminate>>::value, "");

    static_assert(!std::is_same<
        sijson::options<sijson::null_terminate, sijson::throw_on_overflow>,
        sijson::options<sijson::null_terminate, sijson::throw_on_overflow, sijson::no_whitespace>>::value, "");

    static_assert(std::is_same<
        sijson::basic_out_strspan<char, sijson::options<sijson::throw_on_overflow, sijson::null_terminate>>,
        sijson::basic_out_strspan<char, sijson::options<sijson::null_terminate, sijson::throw_on_overflow>>>::value, "");

    std::cout << "\nOpts type test: " << typeid(sijson::basic_out_strspan<char, sijson::options<sijson::throw_on_overflow, sijson::null_terminate>>).name();

    //sijson::in_u16str u16test(u"\"Hello world\n 🏳️‍🌈\"");
    //sijson::raw_reader<sijson::in_u16str> rr(u16test);
    //
    //auto s = rr.read_string();

    std::vector<long long> test_times;

#if TEST_UTF8
    std::vector<unsigned long long> num_input;
    std::vector<unsigned long long> num_output;

    char* u8putbuf = new char[4500000]; // cheating!

    for (int i = 0; i < 1000; ++i)
    {
        auto randutf8 = gen_random_utf8((unsigned)i);
    
        sijson::in_str u8get(sijson::outdata(randutf8));
        //sijson::out_str u8put;
        sijson::unchecked_ostrspanstream u8put(u8putbuf, 4500000);
    
        auto t_begin = std::chrono::high_resolution_clock::now();
        while (!u8get.end())
        {
            sijson::utf_codepoint_t cp = *sijson::utf8_novalidate::take_codepoint(u8get);
            sijson::utf8_novalidate::put_codepoint(u8put, cp);
        }
        auto t_end = std::chrono::high_resolution_clock::now();
        test_times.push_back(std::chrono::duration_cast<std::chrono::microseconds>(t_end - t_begin).count());
        num_input.push_back(randutf8.opos());
        num_output.push_back(u8put.opos());
    
        //std::cout << u8put.str();
    }

    delete[] u8putbuf;
    
    long long total_time3 = std::accumulate(test_times.begin(), test_times.end(), (long long)0);
    long long avg_time3 = total_time3 / test_times.size();
    test_times.clear();

    unsigned long long avg_num_input = std::accumulate(num_input.begin(), num_input.end(), (unsigned long long)0) / num_input.size();
    unsigned long long avg_num_output = std::accumulate(num_output.begin(), num_output.end(), (unsigned long long)0) / num_output.size();


    //std::cout << "\n\nUTF8 benchmark:";
    //std::cout << "\nTotal: " << total_time1 << "us";
    //std::cout << "\nAverage: " << avg_time1 << "us";

    //std::cout << "\n\nUTF8-branchless-decode benchmark:";
    //std::cout << "\nTotal: " << total_time2 << "us";
    //std::cout << "\nAverage: " << avg_time2 << "us";

    std::cout << "\n\nUTF8 v2 benchmark:";
    std::cout << "\nTotal: " << total_time3 << "us";
    std::cout << "\nAverage: " << avg_time3 << "us";
    std::cout << "\nAverage input bytes: " << avg_num_input;
    std::cout << "\nAverage output bytes: " << avg_num_output;
#endif

    std::cout << "\nEscape test: " << sijson::escape("Hello\tworld!");
    std::cout << "\nUnescape test: " << sijson::unescape("Hello\\uD834\\uDD1Eworld!");

#if SIJSON_CPLUSPLUS >= 202002L
    constexpr double d_cexpr = get_value();
    constexpr double d_cepxr2 = 1.0 + 0.0000000000000002220446049250313;
    std::cout << std::setprecision(17) << "\nConstexpr test: " << d_cexpr;
#endif

    // optimization tests (should be no-ops)
    std::cout << "\n\nOptimization tests:\n";

    // test 1
    sijson::in_str test("Hello\\tworld\\t!");
    sijson::out_str otest;
    sijson::raw_reader<sijson::in_str, sijson::no_whitespace> r(test);   
    r.read_string_to(otest, sijson::RDFLAG_str_copy | sijson::RDFLAG_str_only);
    std::cout << otest.view();
    std::cout << "\n";

    // test 2
    sijson::in_str test2("\"Hello\\t\\t!\"");
    sijson::raw_reader<sijson::in_str, sijson::no_whitespace> r2(test2);
    std::cout << r2.read_string();
    std::cout << "\n";

    // test 3
    std::cout << "start test 3";
    sijson::in_str test3("null");
    sijson::raw_reader<sijson::in_str, sijson::no_whitespace> r3(test3);
    r3.read_null();
    std::cout << "\nend test 3";

    // test 4
    sijson::in_str test4("false");
    bool otest4;
    sijson::raw_reader<sijson::in_str, sijson::no_whitespace> r4(test4);
    std::cout << (r4.try_read_bool(otest4) ? "failed read bool" : "read bool");
    std::cout << "\n";

    // test 5
    sijson::in_str test5("false");
    sijson::raw_reader<sijson::in_str, sijson::no_whitespace> r5(test5);
    std::cout << r5.read_bool();
    std::cout << "\n";


    // test 5
    std::cout << sijson::to_value<long long>("-9223372036854775808");

    //try {
    //    throw sijson::parse_error(2, "blah");
    //}
    //catch (sijson::parse_error& e)
    //{
    //    std::cerr << "\n" << e.what();
    //}

    sijson::in_u16str u16test(u"-1.2344999999999999");
    sijson::raw_reader<sijson::in_u16str> u16r(u16test);

    double d = u16r.read_double();
    assert(d == -1.2345);

    
    std::cout << "\n\nPretty print test:\n";
    sijson::in_str ins_pret("[1,1e+17,{\"hello\":\"world\",\"value1\":2147483648,\"-9223372036854775808\":2147483648,\"value2\":-1.2344999999999999}]");
    sijson::pretty_print(ins_pret, std::cout);

    
    
#if TEST_RW
    for (int i = 0; i < 10000; ++i)
    {
        auto t_begin = std::chrono::high_resolution_clock::now();

        //sijson::ofilestream otestfile("test.json");
        //sijson::ascii_writer<sijson::ofilestream> writer(otestfile);

        //std::ofstream otestfile("test.json");
        //sijson::ascii_writer<std::ofstream> writer(otestfile);

        sijson::out_str otestfile;
        sijson::ascii_writer<sijson::out_str> writer(otestfile);

        //char buf[128];
        //sijson::unchecked_ostrspanstream otestfile(buf);
        //sijson::ascii_writer<sijson::unchecked_ostrspanstream> writer(otestfile);

        //std::stringstream otest;
        //sijson::ascii_writer<std::stringstream> writer(otest);

        //sijson::out_stdstr otestfile;
        //sijson::ascii_writer<sijson::out_stdstr> writer(otestfile);

        writer.start_array();
        writer.write_value(1);
        writer.write_value(1e+17);
        writer.start_object();
        writer.write_key_value("hello", "world\nis good \t\t");
        writer.write_key_value("value1", 2147483648);
        writer.write_key_value(sijson::to_string(-9223372036854775807 - 1), 2147483648);
        writer.write_key_value("value2", sijson::number(-1.2345));
        writer.end_object();
        writer.end_array();

        //otestfile.close(); // flush + close


        //sijson::ifilestream itestfile("test.json");
        //sijson::ascii_reader<sijson::ifilestream> reader(itestfile);

        //std::ifstream itest("test.json");
        //sijson::ascii_reader<std::ifstream> reader(itest);

        sijson::in_str itestfile(sijson::outdata(otestfile));
        sijson::ascii_reader<sijson::in_str> reader(itestfile);

        //std::istringstream itest(std::move(otest.str()));
        //sijson::ascii_reader<std::istringstream> reader(itest);

        //sijson::pretty_print(itestfile, std::cout);
        //itestfile.rewind();

        reader.start_array();

        int val1 = reader.read_value<int>();
        assert(val1 == 1); (void)val1;

        double val2 = reader.read_value<double>();
        assert(val2 == 1e+17); (void)val2;

        reader.start_object();

        auto kv1 = reader.read_key_value<std::string>();
        assert(kv1.first == "hello" && kv1.second == "world\nis good \t\t");

        auto kv2 = reader.read_key_value<unsigned long>();
        assert(kv2.first == "value1" && kv2.second == 2147483648uL);

        auto kv = reader.read_key_value<unsigned long>();
        assert(sijson::to_value<long long>(kv.first) == -9223372036854775807 - 1 && kv.second == 2147483648);

        auto kv3 = reader.read_key_value<sijson::number>();
        assert(kv3.first == "value2" && kv3.second.as<double>() == -1.2345);

        reader.end_object();
        
        reader.end_array();

        //itestfile.rewind();

        //sijson::ofilestream otestfile2("test.json", 1024);
        //sijson::out_str otestfile2(1024);
        //sijson::pretty_print(itestfile, otestfile2);
        //sijson::pretty_print(itestfile, std::cout);
        //std::cout << "\n";

        //std::cout.write()

        //sijson::pretty_print(std::cin, std::cout);

        //std::cout << sijson::unescape("blah\n\nblah") << std::endl;

        auto t_end = std::chrono::high_resolution_clock::now();
        test_times.push_back(std::chrono::duration_cast<std::chrono::microseconds>(t_end - t_begin).count());
    }
    
    long long total_time4 = std::accumulate(test_times.begin(), test_times.end(), (long long)0);
    long long avg_time4 = total_time4 / test_times.size();

    std::cout << "\n\nReader/writer benchmark:";
    std::cout << "\nTotal: " << total_time4 << "us";
    std::cout << "\nAverage: " << avg_time4 << "us";
    std::cout << std::endl;
#endif

#ifdef _MSC_VER
    std::system("pause");
#endif
}