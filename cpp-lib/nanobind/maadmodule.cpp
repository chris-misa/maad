#include <nanobind/nanobind.h>

namespace nb = nanobind;
using namespace nb::literals;

int add(int a, int b = 1) {
    return a + b;
}

NB_MODULE(maad, m) {
  m.doc() = "MAAD statistics library";
  m.def("add", &add, "a"_a, "b"_a = 1, "Add two integers");
}