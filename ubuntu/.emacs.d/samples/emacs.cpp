#include <iostream>
#include "fact.h"
#include "combination.h"

using std::cout;
using std::endl;

/*
C(N,k) == m! / (k!*(n-k)!)
 */


int main()
{
  cout << "Hello!" << endl;
  for (int i = 1; i < 10; i++) {
    cout << i << " : " << fact(i) << endl;
  }
    cout <<  combo(5,3) << endl;

  return 0;
}
