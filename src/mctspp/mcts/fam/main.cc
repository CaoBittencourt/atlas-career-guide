#include <stdio.h>

#include <iostream>
#include <string>
#include <type_traits>

template <typename T>
bool _is(T x) {
  return sizeof(x) > 0 ? true : false;
};

class is {
 private:
  // is() {}
  // is(const is &) = delete;
  // is(const is &&) = delete;

 public:
  template <typename T>
  is(T x) {
    return _is(x);
  };
  template <typename T1>
  static bool boolean(T1 x) {
    return std::is_same<T1, bool>::value;
  }

  template <typename T1>
  static bool chr(T1 x) {
    return std::is_same<T1, std::string>::value;
  }
};

int main(const int argc, char const *argv[]) {
  const std::string dsds = "dsds";
  const bool lalala = true;

  if (is::chr(dsds)) {
    std::cout << "\"dsds\" is a string.\n";
  } else {
    std::cout << "\"dsds\" is not a string.\n";
  }

  if (is::boolean(lalala)) {
    std::cout << "\"lalala\" is a boolean.\n";
  } else {
    std::cout << "\"lalala\" is not a boolean.\n";
  }

  if (is::chr(lalala)) {
    std::cout << "\"dsds\" is a string.\n";
  } else {
    std::cout << "\"dsds\" is not a string.\n";
  }

  if (is::boolean(dsds)) {
    std::cout << "\"lalala\" is a boolean.\n";
  } else {
    std::cout << "\"lalala\" is not a boolean.\n";
  }

  is("dsds");

  return 0;
}
