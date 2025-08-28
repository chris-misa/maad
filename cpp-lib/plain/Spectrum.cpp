#include <string>
#include <iostream>
#include <vector>

using namespace std;


int main() {
  double deltaQ = 0.1;
  double minQ = -0.5;
  double maxQ = 3.5;

  vector<double> qs;
  double q = minQ;

  while (q < maxQ) {
    qs.push_back(q);
    q += deltaQ;
  }

  vector<int> prefixLengths;
  int l = 8;

  while (l <= 16) {
    prefixLengths.push_back(l++);    
  }
}