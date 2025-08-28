#include <string>
#include <iostream>
#include <vector>

using namespace std;


int main(int argc, char *argv[]) {
  if (argc != 2) {
    cout << "Usage: " << argv[0] << " <filepath>" << endl;
    return 1;
  }

  string filepath = argv[1];
  double deltaQ = 0.1;
  double minQ = -2.0;
  double maxQ = 4.1;

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