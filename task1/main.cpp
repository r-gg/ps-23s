#include "calculator.h"

//Welcome to the post-fix calculator
//Below is an example of preloading a program in to a register

int main()
{
  Calculator calculator;
  //calculator.disablePrint();
  //calculator.setRegister('a', "(Welcome)\"(((Enter new number:)\"'4!4!*+4!1+/4!1+3!3$3$3$(Current average:)\"2!\")@#1+!@))0 0#1+!@");

  calculator.enablePrint();
  calculator.run();
  return 0;
}
