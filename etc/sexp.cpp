#include "sexp.hpp"

#include <sstream>

using namespace std;

bool parse_char(istream &in, char c) {
  if (in.peek() == c) {
    in.get();
    return true;
  }
  return false;
}

ostream &atom::print(ostream &os) const {
  os << body;
  return os;
}

void slurp_white(istream &in) {
  while (true) {
    char c = in.peek();
    switch (c) {
    case ' ':
    case '\n':
    case '\r':
    case '\t': {
      in.get();
      continue;
    }
    default:
      return;
    }
  }
}

unique_ptr<atom> atom::load(istream & in) {
  string buf;
  slurp_white(in);
  while (true) {
    char c = in.peek();
    switch (c) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
    case '(':
    case ')':
    case char_traits<char>::eof(): {
      if (buf.length() > 0) {
        return make_unique<atom>(buf);
      } else {
        return nullptr;
      }
    }

    default: {
      buf.push_back(in.get());
    }
    }
  }
}

void atom::accept(sexp_visitor & v) const { v.visit(*this); }

ostream &list::print(ostream & os) const {
  os << "(";
  for (auto &elem : body) {
    os << elem << " ";
  }
  os << ")";
  return os;
}

unique_ptr<list> list::load(istream & in) {
  vector<unique_ptr<sexp>> elems;

  if (!parse_char(in, '(')) {
    return nullptr;
  }
  while (true) {
    unique_ptr<sexp> s = sexp::load(in);
    if (!s) {
      break;
    }
    elems.push_back(move(s));
  }
  if (!parse_char(in, ')')) {
    return nullptr;
  }

  return make_unique<list>(elems);
}

void list::accept(sexp_visitor & v) const { v.visit(*this); }

unique_ptr<sexp> sexp::load(istream & in) {
  unique_ptr<list> lout = list::load(in);
  if (lout) {
    return lout;
  }

  return atom::load(in);
}

bool is_atom(const sexp &s) {
  class : public sexp_visitor {
  public:
    bool ret;
    void visit(const atom &) { ret = true; }
    void visit(const list &) { ret = false; }
  } visitor;
  s.accept(visitor);
  return visitor.ret;
}

bool is_list(const sexp &s) { return !is_atom(s); }
