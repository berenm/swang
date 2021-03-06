// RUN: $(dirname %s)/check_clang_tidy.sh %s misc-unused-parameters %t -- -xc
// REQUIRES: shell

// Basic removal
// =============
void a(int i) {}
// CHECK-MESSAGES: :[[@LINE-1]]:12: warning: parameter 'i' is unused [misc-unused-parameters]
// CHECK-FIXES: {{^}}void a(int  /*i*/) {}{{$}}

static void b(); // In C, forward declarations can leave out parameters.
static void b(int i) {}
// CHECK-MESSAGES: :[[@LINE-1]]:19: warning: parameter 'i' is unused [misc-unused-parameters]
// CHECK-FIXES: {{^}}static void b() {}{{$}}

// Unchanged cases
// ===============
void h(i, c, d) int i; char *c, *d; {} // Don't mess with K&R style

