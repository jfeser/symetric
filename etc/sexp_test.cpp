#include "sexp.hpp"

#define BOOST_TEST_MODULE sexp
#include <boost/test/included/unit_test.hpp>

BOOST_AUTO_TEST_CASE(test_atom) {
  auto in = stringstream("test");
  unique_ptr<Atom> x = Atom::load(in);
  BOOST_TEST(x);
  BOOST_TEST(x->get_body() == "test");
}

BOOST_AUTO_TEST_CASE(test_list) {
  auto in = stringstream("(test1 test2)");
  unique_ptr<List> x = List::load(in);
  BOOST_TEST(x);

  class snd : public SexpVisitor {
  public:
    void visit(const List &l) {
      class atom : public SexpVisitor {
        void visit(const List &l) {}
        void visit(const Atom &l) { BOOST_TEST(l.get_body() == "test2"); }
      } visitor;
      l.accept(visitor);
    }

    void visit(const Atom &x) {}
  } visitor;

  x->accept(visitor);
}
