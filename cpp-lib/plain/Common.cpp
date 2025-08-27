#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int preserveUpperBits(int w, int n) { return (w >> (32 - n)) << (32 - n); }

uint32_t string_to_ipv4(const string &str) {
  uint32_t ip = 0;
  uint32_t octet = 0;

  for (char c : str) {
    if (c == '.') {
      ip = (ip << 8) | octet;
      octet = 0;
    } else {
      octet = octet * 10 + (c - '0');
    }
  }

  return (ip << 8) | octet; // Add final octet
}

string ipv4_to_string(uint32_t ip) {
  string result;
  result.reserve(15); // Max IPv4 string length: "255.255.255.255"
  
  result += to_string((ip >> 24) & 0xFF);
  result += '.';
  result += to_string((ip >> 16) & 0xFF);
  result += '.';
  result += to_string((ip >> 8) & 0xFF);
  result += '.';
  result += to_string(ip & 0xFF);
  
  return result;
}

int main() {
  uint32_t test_ip = string_to_ipv4("192.168.1.1");
  cout << "IP as int: " << test_ip << endl;
  cout << "Back to string: " << ipv4_to_string(test_ip) << endl;
  return 0;
}