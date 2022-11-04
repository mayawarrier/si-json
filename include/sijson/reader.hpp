
#ifndef SIJSON_READER_HPP
#define SIJSON_READER_HPP

#include <cstddef>
#include <cstdint>
#include <cassert>
#include <memory>
#include <type_traits>
#include <limits>
#include <ios>
#include <string>
#include <utility>
#include <stdexcept>

#include "internal/util.hpp"
#include "internal/impl_rw.hpp"

#include "core.hpp"
#include "number.hpp"
#include "stringstream.hpp"

#include "raw_reader.hpp"

namespace sijson {


// ASCII JSON reader.
template <typename Istream,
    // Allocator type used for internal purposes/book-keeping.
    typename Allocator = std::allocator<iutil::placeholder>>
class ascii_reader : public internal::rw_base<Allocator>
{
public:
    ascii_reader(Istream& stream) :
        m_rr{ stream }
    {
        this->m_nodes.push({ DOCNODE_root });
    }

    // Get next unread token, skipping any whitespace.
    inline sijson::token token(void) { return m_rr.token(); }

    // Get parent node.
    // For eg. if you call start_object(), parent_node()
    // returns DOCNODE_object until the next call to 
    // end_object() or start_array().
    inline doc_node_type parent_node(void) const { return this->m_nodes.top().type; }

    // Start reading object.
    inline void start_object(void)
    {
        this->template start_node<DOCNODE_object>([&] {
            read_separator();
            m_rr.read_start_object();
        });
    }

    // Start reading array.
    inline void start_array(void)
    {
        this->template start_node<DOCNODE_array>([&] {
            read_separator();
            m_rr.read_start_array();
        });
    }

    // End reading object.
    inline void end_object(void)
    {
        this->template end_node<DOCNODE_object>([&] 
        { m_rr.read_end_object(); });
    }

    // End reading array.
    inline void end_array(void)
    {
        this->template end_node<DOCNODE_array>([&] 
        { m_rr.read_end_array(); });
    }

    // Read object key. String is unescaped.
    inline std::string read_key(void)
    {
        return read_key<std::allocator<char>>();
    }

    // Read object key with custom allocator.
    // String is unescaped.
    template <
        typename StrAllocator = std::allocator<char>>
        inline std::basic_string<char, std::char_traits<char>, StrAllocator> read_key(void);

    // Read object key of custom type.
    // DestString must be a std::basic_string.
    template <typename DestString>
    inline DestString read_key_as(void)
    {
        static_assert(iutil::is_instance_of_basic_string<DestString, char>::value,
            "DestString is not a std::basic_string.");

        return read_key<typename DestString::allocator_type>();
    }

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename ...Ts>
    void read_key(const std::basic_string<char, Ts...>& expected_key);

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    void read_key(const char* expected_key);

    // Read object key.
    // Throws if key (after unescaping) does not match expected_key.
    void read_key(const char* expected_key, std::size_t length);

    // Read value.
    template <typename Value>
    Value read_value(void);

    // Read value.
    template <typename Value>
    inline void read_value(Value& out_value)
    {
        out_value = read_value<Value>();
    }

    // Read object key-value pair.
    // Key must be a std::basic_string.
    template <typename Key, typename Value>
    inline std::pair<Key, Value> read_key_value(void);

    // Read object key-value pair.
    template <typename Value>
    inline std::pair<std::string, Value> read_key_value(void)
    {
        return read_key_value<std::string, Value>();
    }

    // Read object key-value pair.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Value, typename ...Ts>
    inline void read_key_value(const std::basic_string<char, Ts...>& expected_key, Value& out_value)
    {
        read_key(expected_key);
        read_value(out_value);
    }

    // Read object key-value pair.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Value>
    inline void read_key_value(const char* expected_key, Value& out_value)
    {
        read_key(expected_key);
        read_value(out_value);
    }

    // Read object key-value pair.
    // Throws if key (after unescaping) does not match expected_key.
    template <typename Value>
    inline void read_key_value(const char* expected_key, std::size_t key_length, Value& out_value)
    {
        read_key(expected_key, key_length);
        read_value(out_value);
    }

    // Get stream position.
    inline std::size_t inpos(void) { return m_rr.stream().inpos(); }

    // True if reached end of stream.
    inline bool end(void) { return m_rr.stream().end(); }

private:
    inline void read_separator(void);

    template <typename IsEndpFunc>
    inline bool read_key_impl(const char* str, IsEndpFunc is_endp, std::size_t& out_pos);

private:
    raw_reader<Istream> m_rr;
};





namespace internal
{
template <typename Traits, typename IsEndpFunc>
class streq_ostream
{
public:
    streq_ostream(const char* str, IsEndpFunc is_endp) :
        m_equal(true), m_strp(str), m_is_endp(is_endp)
    {}

    inline void put(char c)
    {
        if (!m_equal) return;
        if (!m_is_endp(m_strp))
        {
            m_equal = Traits::eq(*m_strp, c);
            m_strp++;
        }
        else m_equal = false;
    }

    inline void put_n(const char* str, std::size_t count)
    {
        for (std::size_t i = 0; i < count; ++i)
            put(str[i]);
    }

    inline bool str_is_equal(void) const noexcept
    {
        return m_equal && m_is_endp(m_strp);
    }
private:
    bool m_equal;
    const char* m_strp;
    IsEndpFunc m_is_endp;
};
}





template <typename Istream, typename Allocator>
inline void ascii_reader<Istream, Allocator>::read_separator(void)
{
    if (this->m_nodes.top().has_children)
    {
        switch (this->m_nodes.top().type)
        {
            case DOCNODE_object:
            case DOCNODE_array: m_rr.read_item_separator(); break;
            case DOCNODE_root: throw std::runtime_error(this->EXSTR_multi_root);
            default: assert(false);
        }
    }
    // key node is popped before it can have children
    else if (this->m_nodes.top().type == DOCNODE_key)
        m_rr.read_key_separator();
}

template <typename Istream, typename Allocator>
template <typename StrAllocator>
inline std::basic_string<char, std::char_traits<char>, StrAllocator> ascii_reader<Istream, Allocator>::read_key(void)
{
    this->template assert_rule<DOCNODE_key>();

    read_separator();
    auto str = m_rr.template read_string<StrAllocator>();

    this->m_nodes.push({ DOCNODE_key });
    // don't end_child_node(), key-value pair is incomplete
    return str;
}

template <typename Istream, typename Allocator>
template <typename IsEndpFunc>
inline bool ascii_reader<Istream, Allocator>::read_key_impl(const char* str, IsEndpFunc is_endp, std::size_t& out_pos)
{
    this->template assert_rule<DOCNODE_key>();

    read_separator();
    iutil::skip_ws(m_rr.stream(), out_pos);

    internal::streq_ostream<std::char_traits<char>, IsEndpFunc> os(str, is_endp);
    m_rr.read_string_into(os);

    this->m_nodes.push({ DOCNODE_key });
    // don't end_child_node(), key-value pair is incomplete

    return os.str_is_equal();
}

template <typename Istream, typename Allocator>
template <typename ...Ts>
inline void ascii_reader<Istream, Allocator>::read_key(const std::basic_string<char, Ts...>& expected_key)
{
    auto is_endp = [&](const char* p) { return p == expected_key.data() + expected_key.size(); };

    std::size_t startpos;
    if (!read_key_impl(expected_key.data(), is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key.data(), expected_key.length()) + "\"");
}

template <typename Istream, typename Allocator>
inline void ascii_reader<Istream, Allocator>::read_key(const char* expected_key)
{
    auto is_endp = [](const char* p) { return *p == '\0'; };

    std::size_t startpos;
    if (!read_key_impl(expected_key, is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key) + "\"");
}

template <typename Istream, typename Allocator>
inline void ascii_reader<Istream, Allocator>::read_key(const char* expected_key, std::size_t length)
{
    auto is_endp = [&](const char* p) { return p == expected_key + length; };

    std::size_t startpos;
    if (!read_key_impl(expected_key, is_endp, startpos))
        throw iutil::parse_error_exp(startpos, "string \"" + std::string(expected_key, length) + "\"");
}

template <typename Istream, typename Allocator>
template <typename Value>
inline Value ascii_reader<Istream, Allocator>::read_value(void)
{
    this->template assert_rule<DOCNODE_value>();

    read_separator();
    Value value = m_rr.template read<Value>();

    this->end_child_node();
    return value;
}

template <typename Istream, typename Allocator>
template <typename Key, typename Value>
inline std::pair<Key, Value> ascii_reader<Istream, Allocator>::read_key_value(void)
{
    static_assert(iutil::is_instance_of_basic_string<Key, char>::value, 
        "Key must be a std::basic_string with value_type char.");

    using KeyAllocator = typename Key::allocator_type;

    this->template assert_rule<DOCNODE_key, DOCNODE_value>();

    if (this->m_nodes.top().has_children)
        m_rr.read_item_separator();

    Key key = m_rr.template read_string<KeyAllocator>();
    m_rr.read_key_separator();
    Value value = m_rr.template read<Value>();

    this->end_child_node();
    return { std::move(key), std::move(value) };
}

}

#endif
