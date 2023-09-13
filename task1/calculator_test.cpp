#include "calculator.h"
#include <gtest/gtest.h>

// Welcome to the testing of the calculator itself
// every test is written in the calculator syntax and then compared
// to the expected data stack (in some cases more is needed)

// Specification fulfillment tests

TEST(SpecificationTests, RegisterReading)
{
  Calculator calculator;
  calculator.run_test("1 B");
  ASSERT_EQ(std::get<int>(calculator.getRegister('b')), 1);
  ASSERT_TRUE(calculator.outStack.empty());
}

TEST(SpecificationTests, RegisterWriting)
{
  Calculator calculator;
  calculator.setRegister('b', 3);
  calculator.run_test("b");
  ASSERT_EQ(calculator.outStack, "3");
}

TEST(SpecificationTests, NullTest)
{
  Calculator calculator;
  calculator.run_test("()_0_");
  ASSERT_EQ(calculator.outStack, "1 1");
}

TEST(SpecificationTests, NegationTest)
{
  Calculator calculator;
  calculator.run_test("1~1.1~2~~2.2~~(programmiersprachen)~");
  ASSERT_EQ(calculator.outStack, "-1 -1.1 2 2.2 ()");
}

TEST(SpecificationTests, CopyTest)
{
  Calculator calculator;
  calculator.run_test("1 2 3 3!");
  ASSERT_EQ(calculator.outStack, "1 2 3 2");
}

TEST(SpecificationTests, DeleteTest)
{
  Calculator calculator;
  calculator.run_test("1 2 3 3$");
  ASSERT_EQ(calculator.outStack, "2 3");
}

TEST(SpecificationTests, ApplyImmediatelyTest)
{
  Calculator calculator;
  calculator.run_test("(1 2+)@");
  ASSERT_EQ(calculator.outStack, "3");
}

TEST(SpecificationTests, ApplyLaterTest)
{
  Calculator calculator;
  calculator.run_test("(1+)\\ 2");
  ASSERT_EQ(calculator.outStack, "3");
}

TEST(SpecificationTests, StackSizeNonEmptyTest)
{
  Calculator calculator;
  calculator.run_test("1 2#");
  ASSERT_EQ(calculator.outStack, "1 2 2");
}

TEST(SpecificationTests, StackSizeEmptyTest)
{
  Calculator calculator;
  calculator.run_test("#");
  ASSERT_EQ(calculator.outStack, "0");
}

TEST(SpecificationTests, ExampleTest1)
{
  Calculator calculator;
  calculator.run_test("1(8)(9~)(4!4$_1+$@)@");
  ASSERT_EQ(calculator.outStack, "8");
}

TEST(SpecificationTests, ExampleTest2)
{
  Calculator calculator;
  calculator.run_test("3(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$");
  ASSERT_EQ(calculator.outStack, "6");
}

TEST(SpecificationTests, EpsilonTestEqualTrue)
{
  Calculator calculator;
  calculator.run_test("1.1 1.2=");
  ASSERT_EQ(calculator.outStack, "1");
}

TEST(SpecificationTests, EpsilonTestEqualFalse)
{
  Calculator calculator;
  calculator.run_test("1.1 1.3=");
  ASSERT_EQ(calculator.outStack, "0");
}

TEST(SpecificationTests, EpsilonTestLessTrue)
{
  Calculator calculator;
  calculator.run_test("1.1 1.3<");
  ASSERT_EQ(calculator.outStack, "1");
}

TEST(SpecificationTests, EpsilonTestLessFalse)
{
  Calculator calculator;
  calculator.run_test("1.1 1.2<");
  ASSERT_EQ(calculator.outStack, "0");
}

TEST(SpecificationTests, EpsilonTestGreaterTrue)
{
  Calculator calculator;
  calculator.run_test("1.3 1.1>");
  ASSERT_EQ(calculator.outStack, "1");
}

TEST(SpecificationTests, EpsilonTestGreaterFalse)
{
  Calculator calculator;
  calculator.run_test("1.2 1.1>");
  ASSERT_EQ(calculator.outStack, "0");
}

// Integration tests meant to try out multiple functionalities at a time

TEST(IntegrationTests, BasicAddition)
{
  Calculator calculator;
  calculator.run_test("1 2+");
  ASSERT_EQ(calculator.outStack, "3");
}

TEST(IntegrationTests, AdditionAndApplyNow)
{
  Calculator calculator;
  calculator.run_test("(1.222 3+)@ (2.001+)@");
  ASSERT_EQ(calculator.outStack, "6.223");
}

TEST(IntegrationTests, AdditionAndApplyNowAndRegisters)
{
  Calculator calculator;
  calculator.run_test("(1 2+)@ B (b 1+)@ b");
  ASSERT_EQ(calculator.outStack, "4 3");
}

TEST(IntegrationTests, AdditionAndApplyNowAndRegistersWithDelete)
{
  Calculator calculator;
  calculator.run_test("(1 2+)@ B (b 1+)@ b 2 $");
  ASSERT_EQ(calculator.outStack, "3");
}

TEST(IntegrationTests, WorkingWithFloats)
{
  Calculator calculator;
  calculator.run_test("4 2.2+ (2 1/)@ + 3 5 3!++-");
  ASSERT_EQ(calculator.outStack, "-2.8");
}

TEST(IntegrationTests, CopyAndDelete)
{
  Calculator calculator;
  calculator.run_test("1 2 3 4 3 2!$");
  ASSERT_EQ(calculator.outStack, "1 2 4 3");
}
