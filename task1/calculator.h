#pragma once

#include <cctype>
#include <cmath>
#include <iostream>
#include <limits>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>

class Calculator
{
public:
  //calculator architecture
  std::string commandStream;
  int operationMode;
  std::stack<std::variant<int, double, std::string>> dataStack;
  std::unordered_map<char, std::variant<int, double, std::string>> registerSet;
  std::string inputStream;
  std::ostream &outputStream = std::cout;

  double epsilon = 0.1;

  //for testing purposes
  std::variant<int, double, std::string> getRegister(char reg);
  void setRegister(char reg, std::variant<int, double, std::string> value);
  void setInitialDataStack(std::stack<std::variant<int, double, std::string>> stack);
  void run_test(const std::string &input);

  //visual aid
  bool printEnabled = true;
  void disablePrint();
  void enablePrint();
  std::string outStack;
  void printEverything();
  std::ostream &printStack(std::ostream &out, bool brackets,
                           bool endline);
  void printCommandStream();

  private:
  void pushValueToStack(std::variant<int, double, std::string> value);
  std::variant<int, double, std::string> popValueFromStack();
  std::variant<int, double, std::string> topValueFromStack();
  int getStackSize();
  
  template <typename T>
  bool compareNumbersHelper(T first, T second, int mode); // modes (-1: lt, 0: e, 1: gt)
  bool compareNumbersHelperDouble(double first, double second, int mode);
  bool compareNumbers(const std::variant<int, double, std::string> &value1,
                      const std::variant<int, double, std::string> &value2,
                      int mode);
  void performAddition();
  void performSubtraction();
  void performMultiplication();
  void performDivision();
  void performModulus();
  void performLogicalAnd();
  void performLogicalOr();
  void performNullCheck();
  void performNegation();
  void performIntegerConversion();
  void performCopy();
  void performDelete();
  void performApplyImmediately();
  void performApplyLater();
  void performIntegerConstructionMode(char inputChar);
  void performDecimalPlaceConstructionMode(char inputChar);
  void performStringConstructionMode(char inputChar);
  void executeCommand(char inputChar);
  void readInputFromStream();
  void writeOutput();

public:
  Calculator() : operationMode(0) {}
  void run();
};
