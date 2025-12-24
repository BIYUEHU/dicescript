#include <stdlib.h>
#include <time.h>

void idris2_init_random()
{
  srand(time(NULL));
}

double idris2_random()
{
  return (double)rand() / RAND_MAX;
}

int64_t idris2_time()
{
  return (int64_t)time(NULL);
}