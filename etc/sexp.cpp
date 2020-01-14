#include "sexp.hpp"

#include <sstream>

ostream &Atom::print(ostream &os) const {
  os << body;
  return os;
}

unique_ptr<Atom> Atom::load(istream &in) {
  string buf;
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

    case '(':
    case ')':
    case char_traits<char>::eof(): {
      return make_unique<Atom>(buf.str());
    }

    default: {
      buf << in.get();
    }
    }
  }
}

void Atom::accept(SexpVisitor &v) const { v.visit(*this); }

ostream &List::print(ostream &os) const {
  os << "(";
  for (auto &elem : body) {
    os << elem << " ";
  }
  os << ")";
  return os;
}

unique_ptr<List> List::load(istream &in) {
  vector<unique_ptr<Sexp>> elems;

  while (true) {
    unique_ptr<Sexp> s = Sexp::load(in);
    if (s) {
      elems.push_back(move(s));
    } else {
      break;
    }
  }

  return make_unique<List>(elems);
}

void List::accept(SexpVisitor &v) const { v.visit(*this); }

bool parse_char(istream &in, char c) {
  if (in.peek() == c) {
    in.get();
    return true;
  } else {
    return false;
  }
}

unique_ptr<Sexp> Sexp::load(istream &in) {
  if (!parse_char(in, '(')) {
    unique_ptr<List> lout = List::load(in);
    if (!parse_char(in, ')')) {
      return nullptr;
    }
    return lout;
  } else {
    return Atom::load(in);
  }
}
