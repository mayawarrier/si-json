
#ifndef SIJSON_INTERNAL_IMPL_RW_HPP
#define SIJSON_INTERNAL_IMPL_RW_HPP

#include <cassert>
#include <bitset>
#include <string>
#include <stack>
#include <deque>
#include <stdexcept>

#include "core.hpp"
#include "util.hpp"


namespace sijson {
namespace internal {

class rw_util
{
public:
    static constexpr const char* EXSTR_multi_root = "Document cannot have more than one root element.";

protected:
    struct node_info
    {
        const doc_node_type type;
        bool has_children;

        node_info(doc_node_type type) :
            type(type), has_children(false)
        {}
    };

    static inline void assert_type(doc_node_type type, doc_node_type expected)
    {
#if SIJSON_LOGIC_ERRORS
        if (type != expected)
            throw std::logic_error(std::string("Expected parent node to be ") + docnode_name(expected));
#else
        assert(type == expected);
        (void)type;
        (void)expected;
#endif
    }
    static inline void assert_type_before_value(doc_node_type type)
    {
        static constexpr std::bitset<NUM_DOCNODE_TYPES> EXP_before_value
        { 0x1 << DOCNODE_array | 0x1 << DOCNODE_key | 0x1 << DOCNODE_root };

#if SIJSON_LOGIC_ERRORS
        if (!EXP_before_value[type])
            throw std::runtime_error("Expected parent node to be one of array, key, or root.");
#else
        assert(EXP_before_value[type]);
        (void)EXP_before_value;
        (void)type;
#endif
    }

    template <doc_node_type ...types>
    static inline void assert_rule(doc_node_type top_node);
};

template <> inline void rw_util::assert_rule<DOCNODE_array>(doc_node_type type) { assert_type_before_value(type); }
template <> inline void rw_util::assert_rule<DOCNODE_object>(doc_node_type type) { assert_type_before_value(type); }
template <> inline void rw_util::assert_rule<DOCNODE_key>(doc_node_type type) { assert_type(type, DOCNODE_object); }
template <> inline void rw_util::assert_rule<DOCNODE_value>(doc_node_type type) { assert_type_before_value(type); }
template <> inline void rw_util::assert_rule<DOCNODE_key, DOCNODE_value>(doc_node_type type) { assert_type(type, DOCNODE_object); }


template <typename Allocator>
class rw_base : public rw_util
{
protected:
    // Assert that appending these nodes to the
    // document will not make it invalid JSON.
    template <doc_node_type ...types>
    inline void assert_rule(void)
    {
        rw_util::assert_rule<types...>(m_nodes.top().type);
    }

    template <doc_node_type type, typename Func>
    inline void start_node(Func read_or_write_node)
    {
        assert_rule<type>();
        read_or_write_node();
        m_nodes.push({ type });
    }

    inline void end_child_node(void)
    {
        // all values (object/array/primitive) 
        // complete a key-value pair
        if (m_nodes.top().type == DOCNODE_key)
            m_nodes.pop();

        m_nodes.top().has_children = true;
    }

    template <doc_node_type type, typename Func>
    inline void end_node(Func read_or_write_node)
    {
        assert_type(m_nodes.top().type, type);
        m_nodes.pop();
        read_or_write_node();
        end_child_node();
    }

protected:
    std::stack<node_info, std::deque<node_info,
        iutil::rebind_alloc_t<Allocator, node_info>>> m_nodes;
};



}}

#endif