#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

uint64_t choose();

int main() {
  bool t = (3 > 2);
  uint64_t a = 10;
  uint64_t b = 8;
  uint64_t result = choose(t,a,b);

  printf("result: %u\n", result);

  return 0;
}
