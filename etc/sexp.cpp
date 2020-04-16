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

atom* atom::load(istream & in) {
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
        return new atom(buf);
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
    os << *elem << " ";
  }
  os << ")";
  return os;
}

list* list::load(istream & in) {
  vector<sexp*> elems;

  slurp_white(in);
  if (!parse_char(in, '(')) {
    return nullptr;
  }
  while (true) {
    sexp* s = sexp::load(in);
    if (!s) {
      break;
    }
    elems.push_back(s);
  }
  if (!parse_char(in, ')')) {
    return nullptr;
  }

  return new list(elems);
}

void list::accept(sexp_visitor & v) const { v.visit(*this); }

sexp* sexp::load(istream & in) {
  list *lout = list::load(in);
  if (lout) {
    return lout;
  }
  atom *aout = atom::load(in);
  if (aout) {
    return aout;
  }

  return nullptr;
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
