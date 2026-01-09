
// Test that multi-line raw string literals correctly update line tracking
// in preprocessed output. This ensures that line markers (# <line> "<file>")
// are emitted with the correct line numbers after raw strings that span
// multiple lines.
//
// RUN: %clang_cc1 -E -std=c++17 %s -o %t.ii
// RUN: FileCheck %s < %t.ii
// RUN: %clang_cc1 -std=c++17 %t.ii -fsyntax-only

// expected-no-diagnostics

extern void foo(const char *str);
extern void foo(const char16_t *str);
extern void foo(const char32_t *str);
extern void foo(const wchar_t *str);

void bar(void) {
    static_assert(__LINE__ == 19);
    auto str = R"(foo
        bar)";
    static_assert(__LINE__ == 22);
    foo(str);

    static_assert(__LINE__ == 25);
    foo("foo\nbar");
    static_assert(__LINE__ == 27);

    static_assert(__LINE__ == 29);
    auto u_str = uR"(foo
        bar)";
    static_assert(__LINE__ == 32);
    foo(u_str);

    static_assert(__LINE__ == 35);
    foo(u"foo\nbar");
    static_assert(__LINE__ == 37);

    static_assert(__LINE__ == 39);
    auto u8_str = u8R"(foo
        bar)";
    static_assert(__LINE__ == 42);
    foo(u8_str);

    static_assert(__LINE__ == 45);
    foo(u8"foo\nbar");
    static_assert(__LINE__ == 47);

    static_assert(__LINE__ == 49);
    auto u32_str = UR"(foo
        bar)";
    static_assert(__LINE__ == 52);
    foo(u32_str);

    static_assert(__LINE__ == 55);
    foo(U"foo\nbar");
    static_assert(__LINE__ == 57);

    static_assert(__LINE__ == 59);
    auto w_str = LR"(foo
        bar)";
    static_assert(__LINE__ == 62);
    foo(w_str);

    static_assert(__LINE__ == 65);
    foo(L"foo\nbar");
    static_assert(__LINE__ == 67);
}

// CHECK: void bar(void) {

// CHECK: static_assert(19 == 19);
// CHECK-NEXT: auto str = R"(foo
// CHECK-NEXT:    bar)";
// CHECK-NEXT: static_assert(22 == 22);
// CHECK-NEXT: foo(str);

// CHECK: static_assert(25 == 25);
// CHECK: foo("foo\nbar");
// CHECK-NEXT: static_assert(27 == 27);

// CHECK: static_assert(29 == 29);
// CHECK-NEXT: auto u_str = uR"(foo
// CHECK-NEXT:    bar)";
// CHECK-NEXT: static_assert(32 == 32);

// CHECK: static_assert(35 == 35);
// CHECK-NEXT: foo(u"foo\nbar");
// CHECK-NEXT: static_assert(37 == 37);

// CHECK: static_assert(39 == 39);
// CHECK-NEXT: auto u8_str = u8R"(foo
// CHECK-NEXT:    bar)";
// CHECK-NEXT: static_assert(42 == 42);
// CHECK-NEXT: foo(u8_str);

// CHECK: static_assert(45 == 45);
// CHECK-NEXT: foo(u8"foo\nbar");
// CHECK-NEXT: static_assert(47 == 47);

// CHECK: static_assert(49 == 49);
// CHECK-NEXT: auto u32_str = UR"(foo
// CHECK-NEXT:    bar)";
// CHECK-NEXT: static_assert(52 == 52);
// CHECK-NEXT: foo(u32_str);

// CHECK: static_assert(55 == 55);
// CHECK-NEXT: foo(U"foo\nbar");
// CHECK-NEXT: static_assert(57 == 57);

// CHECK: static_assert(59 == 59);
// CHECK-NEXT: auto w_str = LR"(foo
// CHECK-NEXT:    bar)";
// CHECK-NEXT: static_assert(62 == 62);
// CHECK-NEXT: foo(w_str);

// CHECK: static_assert(65 == 65);
// CHECK-NEXT: foo(L"foo\nbar");
// CHECK-NEXT: static_assert(67 == 67);

// CHECK-NEXT: }
