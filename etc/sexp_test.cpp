#include "sexp.hpp"

#define BOOST_TEST_MODULE sexp
#include <boost/test/included/unit_test.hpp>

using std::stringstream;
using std::unique_ptr;

BOOST_AUTO_TEST_CASE(test_atom) {
  auto in = stringstream("test");
  unique_ptr<atom> x = atom::load(in);
  BOOST_TEST(x);
  BOOST_TEST(x->get_body() == "test");
}

BOOST_AUTO_TEST_CASE(test_list) {
  auto in = stringstream("(test1 test2)");
  unique_ptr<list> x = list::load(in);
  BOOST_TEST(x);

  class snd : public sexp_visitor {
  public:
    void visit(const list &l) {
      class atomv : public sexp_visitor {
        void visit(const list &) {}
        void visit(const atom &l) { BOOST_TEST(l.get_body() == "test2"); }
      } visitor;
      l.accept(visitor);
    }

    void visit(const atom &) {}
  } visitor;

  x->accept(visitor);
}
