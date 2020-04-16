#include "sexp.hpp"

#define BOOST_TEST_MODULE sexp
#include <boost/test/included/unit_test.hpp>

using std::stringstream;

BOOST_AUTO_TEST_CASE(test_atom) {
  auto in = stringstream("test");
  atom* x = atom::load(in);
  BOOST_TEST(x);
  BOOST_TEST(x->get_body() == "test");
}

BOOST_AUTO_TEST_CASE(test_list) {
  auto in = stringstream("(test1 test2)");
  list* x = list::load(in);
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

BOOST_AUTO_TEST_CASE(test_two_load) {
  auto in = stringstream("((2 5 9) (8 1 6 2 2 2 5 4) (5 5 1 0 5 0 7) (0 10 1 7 3) (5 4 1 10))\n((1 0 1) (1 0 0 0 0 0 0 1) (0 0 0 0 0 0 0) (0 1 0 1 0) (1 0 0 1))");
  sexp* s1 = sexp::load(in);
  BOOST_TEST(s1);
  sexp* s2 = sexp::load(in);
  BOOST_TEST(s2);

  std::cout << *s1 << std::endl << *s2 << std::endl;
}
