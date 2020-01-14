#ifndef SEXP_H
#define SEXP_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

class atom;
class list;

class sexp_visitor {
public:
  virtual void visit(const atom& x) = 0;
  virtual void visit(const list& x) = 0;
};

class sexp {
public:
  virtual ~sexp(){}
  virtual void accept(sexp_visitor &v) const = 0;
  virtual std::ostream& print(std::ostream& out) const = 0;
  static std::unique_ptr<sexp> load(std::istream &in);
  friend std::ostream &operator<<(std::ostream &os, const sexp &x) { return x.print(os); }
};

class atom : public sexp {
public:
  atom(std::string x) {
    body = x;
  }
  void accept(sexp_visitor &v) const override;
  std::ostream &print(std::ostream &out) const override;
  static std::unique_ptr<atom> load(std::istream &in);
  const std::string& get_body() const {
    return body;
  }

private:
  std::string body;
};

class list : public sexp {
public:
  list(std::vector<std::unique_ptr<sexp>> &x) {
    body = move(x);
  }
  void accept(sexp_visitor &v) const override;
  std::ostream &print(std::ostream &out) const override;
  static std::unique_ptr<list> load(std::istream &in);
  const std::vector<std::unique_ptr<sexp>> &get_body() const {
    return body;
  }

private:
  std::vector<std::unique_ptr<sexp>> body;
};

#endif
