#include <stdio.h>
#include <stdint.h>

uint64_t loop();

int main() {
  uint64_t sec = 10;
  uint64_t result = 2;
  uint64_t output = loop(sec,result);

  printf("result: %u\n", output);


  return 0;
}
