
void bar() {
  foo(1, FOO);
  foo(1, FOOBAR(1,2));
  foo(1, cp);
}

struct y {
  struct x a;
};

void use_struct_x(struct x o) {
  printf("%d", o.a);
}
