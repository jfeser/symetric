#include "sexp.hpp"

#include <sstream>

bool parse_char(istream &in, char c) {
  if (in.peek() == c) {
    in.get();
    return true;
  }
  return false;
}

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
      if (buf.length() > 0) {
        return make_unique<Atom>(buf);
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

  if (!parse_char(in, '(')) {
    return nullptr;
  }
  while (true) {
    unique_ptr<Sexp> s = Sexp::load(in);
    if (!s) {
      break;
    }
    elems.push_back(move(s));
  }
  if (!parse_char(in, ')')) {
    return nullptr;
  }

  return make_unique<List>(elems);
}

void List::accept(SexpVisitor &v) const { v.visit(*this); }

unique_ptr<Sexp> Sexp::load(istream &in) {
  unique_ptr<List> lout = List::load(in);
  if (lout) {
    return lout;
  }

  return Atom::load(in);
}
