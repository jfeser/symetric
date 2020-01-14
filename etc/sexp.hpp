#ifndef SEXP_H
#define SEXP_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

using namespace std;

class Atom;
class List;

class SexpVisitor {
public:
  virtual void visit(const Atom& x) = 0;
  virtual void visit(const List& x) = 0;
};

class Sexp {
public:
  virtual ~Sexp(){}
  virtual void accept(SexpVisitor &v) const = 0;
  virtual ostream& print(ostream& out) const = 0;
  static unique_ptr<Sexp> load(istream &in);
  friend ostream &operator<<(ostream &os, const Sexp &x) { return x.print(os); }
};

class Atom : public Sexp {
public:
  Atom(string x) {
    body = x;
  }
  void accept(SexpVisitor &v) const override;
  ostream &print(ostream &out) const override;
  static unique_ptr<Atom> load(istream &in);
  const string& get_body() const {
    return body;
  }

private:
  string body;
};

class List : public Sexp {
public:
  List(vector<unique_ptr<Sexp>> &x) {
    body = move(x);
  }
  void accept(SexpVisitor &v) const override;
  ostream &print(ostream &out) const override;
  static unique_ptr<List> load(istream &in);
  const vector<unique_ptr<Sexp>> &get_body() const {
    return body;
  }

private:
  vector<unique_ptr<Sexp>> body;
};

#endif
