
#ifndef SIJSON_INTERNAL_IMPL_RW_HPP
#define SIJSON_INTERNAL_IMPL_RW_HPP

#include <cassert>
#include <bitset>
#include <string>
#include <stack>
#include <deque>
#include <stdexcept>

#include "core.hpp"


namespace sijson {
namespace internal {

struct node_info
{
    const docnode type;
    bool has_children;

    constexpr node_info(docnode type) :
        type(type), has_children(false)
    {}
};

class rw_util
{
public:
    static constexpr char EXSTR_multi_root[] = "Document cannot have more than one root element.";

protected:
    static inline void assert_type(docnode type, docnode expected)
    {
#if SIJSON_USE_LOGIC_ERRORS
        if (type != expected)
            throw std::runtime_error(std::string("Expected parent node to be ") + docnode_name(expected));
#else
        SIJSON_ASSERT(type == expected);
        (void)type;
        (void)expected;
#endif
    }
    static inline void assert_type_before_value(docnode type)
    {
        static constexpr std::bitset<NUM_DOCNODES> EXP_before_value
        { 0x1 << DOCNODE_array | 0x1 << DOCNODE_key | 0x1 << DOCNODE_root };

#if SIJSON_USE_LOGIC_ERRORS
        if (!EXP_before_value[type])
            throw std::runtime_error("Expected parent node to be one of array, key, or root.");
#else
        SIJSON_ASSERT(EXP_before_value[type]);
        (void)EXP_before_value;
        (void)type;
#endif
    }

    template <docnode ...types>
    static inline void assert_rule(docnode top_node);
};

template <> inline void rw_util::assert_rule<DOCNODE_array>(docnode type) { assert_type_before_value(type); }
template <> inline void rw_util::assert_rule<DOCNODE_object>(docnode type) { assert_type_before_value(type); }
template <> inline void rw_util::assert_rule<DOCNODE_key>(docnode type) { assert_type(type, DOCNODE_object); }
template <> inline void rw_util::assert_rule<DOCNODE_value>(docnode type) { assert_type_before_value(type); }
template <> inline void rw_util::assert_rule<DOCNODE_key, DOCNODE_value>(docnode type) { assert_type(type, DOCNODE_object); }


template <typename Allocator>
class rw_base : public rw_util
{
protected:
    // Assert that appending these nodes to the
    // document will not make it invalid JSON.
    template <docnode ...types>
    inline void assert_rule(void)
    {
        rw_util::assert_rule<types...>(m_nodes.top().type);
    }

    template <docnode type, typename Func>
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

    template <docnode type, typename Func>
    inline void end_node(Func read_or_write_node)
    {
        assert_type(m_nodes.top().type, type);
        m_nodes.pop();
        read_or_write_node();
        end_child_node();
    }

protected:
    // some reading: http://www.gotw.ca/gotw/054.htm
    std::stack<node_info, std::deque<node_info,
        iutil::rebind_alloc_t<Allocator, node_info>>> m_nodes;
};



}}

#endif