// RUN: $(dirname %s)/check_clang_tidy.sh %s modernize-pass-by-value %t -- -std=c++11 -fno-delayed-template-parsing
// REQUIRES: shell

// CHECK-FIXES: #include <utility>

namespace {
// POD types are trivially move constructible.
struct Movable {
  int a, b, c;
};

struct NotMovable {
  NotMovable() = default;
  NotMovable(const NotMovable &) = default;
  NotMovable(NotMovable &&) = delete;
  int a, b, c;
};
}

struct A {
  A(const Movable &M) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move [modernize-pass-by-value]
  // CHECK-FIXES: A(Movable M) : M(std::move(M)) {}
  Movable M;
};

// Test that we aren't modifying other things than a parameter.
Movable GlobalObj;
struct B {
  B(const Movable &M) : M(GlobalObj) {}
  // CHECK-FIXES: B(const Movable &M) : M(GlobalObj) {}
  Movable M;
};

// Test that a parameter with more than one reference to it won't be changed.
struct C {
  // Tests extra-reference in body.
  C(const Movable &M) : M(M) { this->i = M.a; }
  // CHECK-FIXES: C(const Movable &M) : M(M) { this->i = M.a; }

  // Tests extra-reference in init-list.
  C(const Movable &M, int) : M(M), i(M.a) {}
  // CHECK-FIXES: C(const Movable &M, int) : M(M), i(M.a) {}
  Movable M;
  int i;
};

// Test that both declaration and definition are updated.
struct D {
  D(const Movable &M);
  // CHECK-FIXES: D(Movable M);
  Movable M;
};
D::D(const Movable &M) : M(M) {}
// CHECK-MESSAGES: :[[@LINE-1]]:6: warning: pass by value and use std::move
// CHECK-FIXES: D::D(Movable M) : M(std::move(M)) {}

// Test with default parameter.
struct E {
  E(const Movable &M = Movable()) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: E(Movable M = Movable()) : M(std::move(M)) {}
  Movable M;
};

// Test with object that can't be moved.
struct F {
  F(const NotMovable &NM) : NM(NM) {}
  // CHECK-FIXES: F(const NotMovable &NM) : NM(NM) {}
  NotMovable NM;
};

// Test unnamed parameter in declaration.
struct G {
  G(const Movable &);
  // CHECK-FIXES: G(Movable );
  Movable M;
};
G::G(const Movable &M) : M(M) {}
// CHECK-MESSAGES: :[[@LINE-1]]:6: warning: pass by value and use std::move
// CHECK-FIXES: G::G(Movable M) : M(std::move(M)) {}

// Test parameter with and without qualifier.
namespace ns_H {
typedef ::Movable HMovable;
}
struct H {
  H(const ns_H::HMovable &M);
  // CHECK-FIXES: H(ns_H::HMovable M);
  ns_H::HMovable M;
};
using namespace ns_H;
H::H(const HMovable &M) : M(M) {}
// CHECK-MESSAGES: :[[@LINE-1]]:6: warning: pass by value and use std::move
// CHECK-FIXES: H(HMovable M) : M(std::move(M)) {}

// Try messing up with macros.
#define MOVABLE_PARAM(Name) const Movable & Name
// CHECK-FIXES: #define MOVABLE_PARAM(Name) const Movable & Name
struct I {
  I(MOVABLE_PARAM(M)) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: I(MOVABLE_PARAM(M)) : M(M) {}
  Movable M;
};
#undef MOVABLE_PARAM

// Test that templates aren't modified.
template <typename T> struct J {
  J(const T &M) : M(M) {}
  // CHECK-FIXES: J(const T &M) : M(M) {}
  T M;
};
J<Movable> j1(Movable());
J<NotMovable> j2(NotMovable());

struct K_Movable {
  K_Movable() = default;
  K_Movable(const K_Movable &) = default;
  K_Movable(K_Movable &&o) { dummy = o.dummy; }
  int dummy;
};

// Test with movable type with an user defined move constructor.
struct K {
  K(const K_Movable &M) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: K(K_Movable M) : M(std::move(M)) {}
  K_Movable M;
};

template <typename T> struct L {
  L(const Movable &M) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: L(Movable M) : M(std::move(M)) {}
  Movable M;
};
L<int> l(Movable());

// Test with a non-instantiated template class.
template <typename T> struct N {
  N(const Movable &M) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: N(Movable M) : M(std::move(M)) {}

  Movable M;
  T A;
};

// Test with value parameter.
struct O {
  O(Movable M) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: O(Movable M) : M(std::move(M)) {}
  Movable M;
};

// Test with a const-value parameter.
struct P {
  P(const Movable M) : M(M) {}
  // CHECK-FIXES: P(const Movable M) : M(M) {}
  Movable M;
};

// Test with multiples parameters where some need to be changed and some don't.
// need to.
struct Q {
  Q(const Movable &A, const Movable &B, const Movable &C, double D)
      : A(A), B(B), C(C), D(D) {}
  // CHECK-MESSAGES: :[[@LINE-2]]:23: warning: pass by value and use std::move
  // CHECK-MESSAGES: :[[@LINE-3]]:41: warning: pass by value and use std::move
  // CHECK-FIXES:      Q(const Movable &A, Movable B, Movable C, double D)
  // CHECK-FIXES:     : A(A), B(std::move(B)), C(std::move(C)), D(D) {}
  const Movable &A;
  Movable B;
  Movable C;
  double D;
};

// Test that value-parameters with a nested name specifier are left as-is.
namespace ns_R {
typedef ::Movable RMovable;
}
struct R {
  R(ns_R::RMovable M) : M(M) {}
  // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: pass by value and use std::move
  // CHECK-FIXES: R(ns_R::RMovable M) : M(std::move(M)) {}
  ns_R::RMovable M;
};

// Test with rvalue parameter.
struct S {
  S(Movable &&M) : M(M) {}
  // CHECK-FIXES: S(Movable &&M) : M(M) {}
  Movable M;
};

