#include "combination.h"
#include "fact.h"

int combo(int n, int k){
  int num = fact(n);
  int den = fact(k)*fact(n-k);

  return num/den;
}
