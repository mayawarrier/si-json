
#ifndef SIJSON_INTERNAL_IMPL_RW_HPP
#define SIJSON_INTERNAL_IMPL_RW_HPP

#include <cassert>
#include <bitset>
#include <type_traits>
#include <memory>
#include <iostream>
#include <string>
#include <stack>
#include <deque>
#include <stdexcept>

#include "common.hpp"
#include "util.hpp"


namespace sijson {
namespace internal {

class rw_util
{
protected:
    struct node_info
    {
        const doc_node_t type;
        bool has_children;

        node_info(doc_node_t type) :
            type(type), has_children(false)
        {
            assert(type < NUM_DOCNODE_TYPES);
        }
    };

    static inline std::runtime_error
        bad_top_node_error(const std::bitset<NUM_DOCNODE_TYPES> expected)
    {
        std::string msg = "Expected parent node to be ";
        if (expected.count() > 1)
            msg += "one of ";

        for (std::size_t i = 0; i < expected.size(); ++i)
        {
            if (!msg.empty()) msg += ", ";
            if (expected[i])
                msg += doc_node_name((doc_node_t)i);
        }
        return std::runtime_error(msg);
    }

    static inline std::runtime_error bad_top_node_error(doc_node_t expected)
    {
        return std::runtime_error(
            std::string("Expected parent node to be ") + doc_node_name(expected));
    }

    static inline void assert_type(doc_node_t type,
        const std::bitset<NUM_DOCNODE_TYPES> expected)
    {
        if (!expected[type])
            throw bad_top_node_error(expected);
    }

    static inline void assert_type(doc_node_t type, doc_node_t expected)
    {
        if (type != expected)
            throw bad_top_node_error(expected);
    }

    template <doc_node_t ...types>
    static inline void assert_rule(doc_node_t top_node);
};

static constexpr std::bitset<NUM_DOCNODE_TYPES> EXP_before_value
{ 0x1 << DOCNODE_array | 0x1 << DOCNODE_key | 0x1 << DOCNODE_root };

template <> inline void rw_util::assert_rule<DOCNODE_array>(doc_node_t type) { assert_type(type, EXP_before_value); }
template <> inline void rw_util::assert_rule<DOCNODE_object>(doc_node_t type) { assert_type(type, EXP_before_value); }
template <> inline void rw_util::assert_rule<DOCNODE_key>(doc_node_t type) { assert_type(type, DOCNODE_object); }
template <> inline void rw_util::assert_rule<DOCNODE_value>(doc_node_t type) { assert_type(type, EXP_before_value); }
template <> inline void rw_util::assert_rule<DOCNODE_key, DOCNODE_value>(doc_node_t type) { assert_type(type, DOCNODE_object); }



template <typename AllocatorPolicy>
class rw_base : public rw_util
{
protected:
    // Assert that appending these nodes to the
    // document will not make it invalid JSON.
    template <doc_node_t ...types>
    inline void assert_rule(void)
    {
        rw_util::assert_rule<types...>(m_nodes.top().type);
    }

    template <doc_node_t type, typename Func>
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

    template <doc_node_t type, typename Func>
    inline void end_node(Func read_or_write_node)
    {
        assert_type(m_nodes.top().type, type);
        m_nodes.pop();
        read_or_write_node();
        end_child_node();
    }

protected:
    std::stack<node_info, std::deque<node_info,
        iutil::rebind_alloc_t<AllocatorPolicy, node_info>>> m_nodes;
};

static const char EXSTR_multi_root[] = "Document cannot have more than one root element.";

}}

#endif