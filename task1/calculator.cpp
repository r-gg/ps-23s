#include "calculator.h"

std::variant<int, double, std::string> Calculator::getRegister(char reg)
{
    return registerSet[reg];
}

void Calculator::setRegister(char reg, std::variant<int, double, std::string> value)
{
    registerSet[reg] = value;
}

void Calculator::setInitialDataStack(std::stack<std::variant<int, double, std::string>> stack)
{
    dataStack = stack;
}

void Calculator::printEverything()
{
    std::cout << "==>";
    printStack(std::cout, false, false);
    std::cout << "    <|    ";
    std::cout << commandStream << std::endl;
}

std::ostream &Calculator::printStack(std::ostream &out, bool brackets = true,
                                     bool endline = true)
{
    std::stack<std::variant<int, double, std::string>> tempStack;
    std::stack<std::variant<int, double, std::string>> tempStack2 = dataStack;
    while (!tempStack2.empty())
    {
        tempStack.push(tempStack2.top());
        tempStack2.pop();
    }
    if (brackets)
        out << "Stack: [";
    while (!tempStack.empty())
    {
        std::visit(
            [tempStack, &out](const auto &value)
            {
                if (std::holds_alternative<std::string>(tempStack.top()))
                    out << "(" << value << ")";
                else
                    out << value;
                if (tempStack.size() > 1)
                    out << " ";
            },
            tempStack.top());
        tempStack.pop();
    }
    if (brackets)
        out << "]";
    if (endline)
        out << std::endl;

    return out;
}

void Calculator::printCommandStream()
{
    std::cout << "Command Stream: " << commandStream << std::endl;
}

void Calculator::pushValueToStack(std::variant<int, double, std::string> value)
{
    dataStack.push(value);
}

std::variant<int, double, std::string> Calculator::popValueFromStack()
{
    if (!dataStack.empty())
    {
        auto topValue = dataStack.top();
        dataStack.pop();
        return topValue;
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

std::variant<int, double, std::string> Calculator::topValueFromStack()
{
    if (!dataStack.empty())
    {
        auto topValue = dataStack.top();
        return topValue;
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

int Calculator::getStackSize() { return dataStack.size(); }

void Calculator::disablePrint() { printEnabled = false; }
void Calculator::enablePrint() { printEnabled = true; } 

// modes (-1: lt, 0: e, 1: gt)
template <typename T>
bool Calculator::compareNumbersHelper(T first, T second, int mode)
{
    if (mode < 0)
        return first < second;
    else if (mode > 0)
        return first > second;
    else
        return first == second;
}

bool Calculator::compareNumbersHelperDouble(double first, double second, int mode)
{
    if (mode < 0)
        return second - first > epsilon; // first < second
    else if (mode > 0)
        return first - second > epsilon; // first > second
    else
        return fabs(first - second) <= epsilon; // first == second
}

bool Calculator::compareNumbers(const std::variant<int, double, std::string> &value1,
                                const std::variant<int, double, std::string> &value2,
                                int mode = 0)
{
    if (std::holds_alternative<int>(value1) &&
        std::holds_alternative<int>(value2))
    {
        return compareNumbersHelper(std::get<int>(value1), std::get<int>(value2),
                                    mode);
    }
    else if (std::holds_alternative<double>(value1) &&
             std::holds_alternative<double>(value2))
    {
        return compareNumbersHelperDouble(std::get<double>(value1),
                                          std::get<double>(value2), mode);
    }
    else if (std::holds_alternative<double>(value1) &&
             std::holds_alternative<int>(value2))
    {
        return compareNumbers(value1, static_cast<double>(std::get<int>(value2)));
    }
    else if (std::holds_alternative<int>(value1) &&
             std::holds_alternative<double>(value2))
    {
        return compareNumbers(static_cast<double>(std::get<int>(value1)), value2);
    }
    return false;
}

void Calculator::performAddition()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (std::holds_alternative<int>(value1) &&
                std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::get<int>(value1) + std::get<int>(value2));
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::get<double>(value1) + std::get<double>(value2));
            }
            else if (std::holds_alternative<std::string>(value1) ||
                     std::holds_alternative<std::string>(value2))
            {
                pushValueToStack(std::get<std::string>(value1) +
                                 std::get<std::string>(value2));
            }
            else if (std::holds_alternative<int>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::get<int>(value1) + std::get<double>(value2));
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::get<double>(value1) + std::get<int>(value2));
            }
            else
            {
                throw std::runtime_error("Invalid operands for addition");
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for addition");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performSubtraction()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (std::holds_alternative<int>(value1) &&
                std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::get<int>(value1) - std::get<int>(value2));
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::get<double>(value1) - std::get<double>(value2));
            }
            else if (std::holds_alternative<int>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::get<int>(value1) - std::get<double>(value2));
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::get<double>(value1) - std::get<int>(value2));
            }
            else
            {
                throw std::runtime_error("Invalid operands for subtraction");
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for subtraction");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performMultiplication()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (std::holds_alternative<int>(value1) &&
                std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::get<int>(value1) * std::get<int>(value2));
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::get<double>(value1) * std::get<double>(value2));
            }
            else if (std::holds_alternative<int>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::get<int>(value1) * std::get<double>(value2));
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::get<double>(value1) * std::get<int>(value2));
            }
            else
            {
                throw std::runtime_error("Invalid operands for multiplication");
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for multiplication");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performDivision()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (std::holds_alternative<int>(value1) &&
                std::holds_alternative<int>(value2))
            {
                if (std::get<int>(value2) != 0)
                {
                    pushValueToStack(std::get<int>(value1) / std::get<int>(value2));
                }
                else
                {
                    pushValueToStack(std::string());
                }
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                if (std::get<double>(value2) != 0.0)
                {
                    pushValueToStack(std::get<double>(value1) /
                                     std::get<double>(value2));
                }
                else
                {
                    pushValueToStack(std::string());
                }
            }
            else if (std::holds_alternative<int>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                if (std::get<double>(value2) != 0.0)
                {
                    pushValueToStack(static_cast<double>(std::get<int>(value1)) /
                                     std::get<double>(value2));
                }
                else
                {
                    pushValueToStack(std::string());
                }
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<int>(value2))
            {
                if (std::get<int>(value2) != 0)
                {
                    pushValueToStack(std::get<double>(value1) / std::get<int>(value2));
                }
                else
                {
                    pushValueToStack(std::string());
                }
            }
            else
            {
                throw std::runtime_error("Invalid operands for division");
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for division");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performModulus()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (std::holds_alternative<int>(value1) &&
                std::holds_alternative<int>(value2))
            {
                if (std::get<int>(value2) != 0)
                {
                    pushValueToStack(std::get<int>(value1) % std::get<int>(value2));
                }
                else
                {
                    pushValueToStack(std::string());
                }
            }
            else if (std::holds_alternative<double>(value1) &&
                     std::holds_alternative<double>(value2))
            {
                pushValueToStack(std::string());
            }
            else
            {
                throw std::runtime_error("Invalid operands for modulus");
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for modulus");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performLogicalAnd()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (!std::holds_alternative<int>(value1) ||
                !std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::string());
            }
            else
            {
                pushValueToStack(std::get<int>(value1) && std::get<int>(value2));
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for logical AND");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performLogicalOr()
{
    if (!dataStack.empty())
    {
        auto value2 = popValueFromStack();
        if (!dataStack.empty())
        {
            auto value1 = popValueFromStack();
            if (!std::holds_alternative<int>(value1) ||
                !std::holds_alternative<int>(value2))
            {
                pushValueToStack(std::string());
            }
            else
            {
                pushValueToStack(std::get<int>(value1) || std::get<int>(value2));
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for logical OR");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performNullCheck()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            pushValueToStack((int)std::get<int>(topValue) == 0);
        }
        else if (std::holds_alternative<double>(topValue))
        {
            pushValueToStack((int)std::get<double>(topValue) > -epsilon &&
                             (int)std::get<double>(topValue) < epsilon);
        }
        else if (std::holds_alternative<std::string>(topValue) &&
                 std::get<std::string>(topValue).empty())
        {
            pushValueToStack(1);
        }
        else
        {
            pushValueToStack(std::string());
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performNegation()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            pushValueToStack(-std::get<int>(topValue));
        }
        else if (std::holds_alternative<double>(topValue))
        {
            pushValueToStack(-std::get<double>(topValue));
        }
        else
        {
            pushValueToStack("");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performIntegerConversion()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<double>(topValue))
        {
            pushValueToStack(static_cast<int>(std::get<double>(topValue)));
        }
        else
        {
            pushValueToStack(std::string());
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performCopy()
{
    if (!dataStack.empty())
    {
        auto topValue = topValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            int n = std::get<int>(topValue);
            if (n >= 0 && n <= getStackSize())
            {
                std::stack<std::variant<int, double, std::string>> tempStack =
                    dataStack;
                for (int i = 1; i < n; i++)
                {
                    tempStack.pop();
                }
                auto copiedValue = tempStack.top();
                popValueFromStack();
                pushValueToStack(copiedValue);
            }
            else
            {
                throw std::runtime_error("Invalid index for copy");
            }
        }
        else
        {
            throw std::runtime_error("Invalid value for copy");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performDelete()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            int n = std::get<int>(topValue);
            if (n >= 0 && n <= getStackSize())
            {
                std::stack<std::variant<int, double, std::string>> tempStack;
                for (int i = 1; i < n; i++)
                {
                    tempStack.push(dataStack.top());
                    dataStack.pop();
                }
                dataStack.pop(); // Remove the nth element
                while (!tempStack.empty())
                {
                    dataStack.push(tempStack.top());
                    tempStack.pop();
                }
            }
            else
            {
                throw std::runtime_error("Invalid index for delete");
            }
        }
        else
        {
            throw std::runtime_error("Invalid value for delete");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performApplyImmediately()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<std::string>(topValue))
        {
            std::string str = std::get<std::string>(topValue);
            for (int i = static_cast<int>(str.length()) - 1; i >= 0; i--)
            {
                commandStream = str[i] + commandStream;
            }
        }
        else
        {
            throw std::runtime_error("Invalid value for apply immediately");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performApplyLater()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<std::string>(topValue))
        {
            std::string str = std::get<std::string>(topValue);
            commandStream += "(" + str + ")@";
        }
        else
        {
            throw std::runtime_error("Invalid value for apply later");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::performIntegerConstructionMode(char inputChar)
{
    if (std::isdigit(inputChar))
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            int integerValue = std::get<int>(topValue) * 10 + (inputChar - '0');
            pushValueToStack(integerValue);
        }
        else
        {
            throw std::runtime_error(
                "Invalid top value for integer construction mode");
        }
    }
    else if (inputChar == '.')
    {
        auto topValue = topValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            operationMode = -2; // Switch to decimal place construction mode
        }
        else
        {
            throw std::runtime_error(
                "Invalid top value for integer construction mode");
        }
    }
    else
    {
        operationMode = 0; // Switch to execution mode
        executeCommand(inputChar);
    }
}

void Calculator::performDecimalPlaceConstructionMode(char inputChar)
{
    if (std::isdigit(inputChar))
    {
        auto topValue = popValueFromStack();

        double decimalValue;
        if (std::holds_alternative<int>(topValue))
            decimalValue = static_cast<double>(std::get<int>(topValue));
        else
            decimalValue = std::get<double>(topValue);
        int numDigitsAfterDecimal = operationMode * -1 - 1;
        decimalValue += (inputChar - '0') * std::pow(10, -numDigitsAfterDecimal);
        pushValueToStack(decimalValue);
        operationMode--; // Move to the next decimal place
    }
    else if (inputChar == '.')
    {
        pushValueToStack(
            0.0);           // Push 0.0 onto the stack for the next decimal place
        operationMode = -2; // Stay in decimal place construction mode
    }
    else
    {
        operationMode = 0; // Switch to execution mode
        executeCommand(inputChar);
    }
}

void Calculator::performStringConstructionMode(char inputChar)
{
    if (inputChar == '(')
    {
        // check if the value on top of the stack is open bracket, if yes modify that value by adding the open bracket
        auto topValue = popValueFromStack();
        topValue = std::get<std::string>(topValue) + "(";
        pushValueToStack(topValue);
        operationMode++; // Move to the next level of string construction
    }
    else if (inputChar == ')')
    {
        if (operationMode > 1)
        {
            auto topValue = popValueFromStack();
            if (std::holds_alternative<std::string>(topValue))
            {
                std::string str = std::get<std::string>(topValue) + ")";
                pushValueToStack(str);
            }
            else
            {
                throw std::runtime_error(
                    "Invalid top value for string construction mode");
            }
        }
        operationMode--; // Move back to the previous level of string
                         // construction
    }
    else
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<std::string>(topValue))
        {
            std::string str = std::get<std::string>(topValue) + inputChar;
            pushValueToStack(str);
        }
        else
        {
            throw std::runtime_error(
                "Invalid top value for string construction mode");
        }
    }
}

void Calculator::executeCommand(char inputChar)
{
    if (std::isdigit(inputChar))
    {
        pushValueToStack(inputChar - '0');
        operationMode = -1; // Switch to integer construction mode
    }
    else if (inputChar == '.')
    {
        pushValueToStack(0.0);
        operationMode = -2; // Switch to decimal place construction mode
    }
    else if (inputChar == '(')
    {
        pushValueToStack("");
        operationMode = 1; // Switch to string construction mode
    }
    else if (std::islower(inputChar))
    {
        int registerIndex = inputChar - 'a';
        if (registerIndex >= 0 && registerIndex <= 25)
        {
            pushValueToStack(registerSet[inputChar]);
        }
        else
        {
            throw std::runtime_error("Invalid register index");
        }
    }
    else if (std::isupper(inputChar))
    {
        int registerIndex = inputChar - 'A';
        if (registerIndex >= 0 && registerIndex <= 25)
        {
            registerSet[tolower(inputChar)] = popValueFromStack();
        }
        else
        {
            throw std::runtime_error("Invalid register index");
        }
    }
    else if (inputChar == '=' || inputChar == '<' || inputChar == '>')
    {
        if (getStackSize() >= 2)
        {
            auto value2 = popValueFromStack();
            auto value1 = popValueFromStack();
            if (inputChar == '=')
            {
                pushValueToStack(compareNumbers(value1, value2) ? 1 : 0);
            }
            else if (inputChar == '<')
            {
                pushValueToStack(compareNumbers(value1, value2, -1) ? 1 : 0);
            }
            else if (inputChar == '>')
            {
                pushValueToStack(compareNumbers(value1, value2, 1) ? 1 : 0);
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for comparison operation");
        }
    }
    else if (inputChar == '+' || inputChar == '-' || inputChar == '*' ||
             inputChar == '/' || inputChar == '%')
    {
        if (getStackSize() >= 2)
        {
            if (inputChar == '+')
            {
                performAddition();
            }
            else if (inputChar == '-')
            {
                performSubtraction();
            }
            else if (inputChar == '*')
            {
                performMultiplication();
            }
            else if (inputChar == '/')
            {
                performDivision();
            }
            else if (inputChar == '%')
            {
                performModulus();
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for arithmetic operation");
        }
    }
    else if (inputChar == '&' || inputChar == '|')
    {
        if (getStackSize() >= 2)
        {
            if (inputChar == '&')
            {
                performLogicalAnd();
            }
            else if (inputChar == '|')
            {
                performLogicalOr();
            }
        }
        else
        {
            throw std::runtime_error(
                "Data stack does not have enough entries for logical operation");
        }
    }
    else if (inputChar == '_')
    {
        performNullCheck();
    }
    else if (inputChar == '~')
    {
        performNegation();
    }
    else if (inputChar == '?')
    {
        performIntegerConversion();
    }
    else if (inputChar == '!')
    {
        performCopy();
    }
    else if (inputChar == '$')
    {
        performDelete();
    }
    else if (inputChar == '@')
    {
        performApplyImmediately();
    }
    else if (inputChar == '\\')
    {
        performApplyLater();
    }
    else if (inputChar == '#')
    {
        pushValueToStack(getStackSize());
    }
    else if (inputChar == '\'')
    {
        readInputFromStream();
    }
    else if (inputChar == '"')
    {
        writeOutput();
    }
    else
    {
        // Do nothing for any other characters
    }
}

void Calculator::readInputFromStream()
{
    std::string input;
    std::getline(std::cin, input);

    // Convert the input to the appropriate type (integer, floating-point
    // number, or string)
    try
    {
        size_t pos;
        int intValue = std::stoi(input, &pos);
        if (pos == input.size())
        {
            pushValueToStack(intValue);
            return;
        }

        double doubleValue = std::stod(input, &pos);
        if (pos == input.size())
        {
            pushValueToStack(doubleValue);
            return;
        }

        // If the input cannot be converted to a number, treat it as a string
        pushValueToStack(input);
    }
    catch (std::invalid_argument &)
    {
        // Input is not a number, treat it as a string
        pushValueToStack(input);
    }
    catch (std::out_of_range &)
    {
        throw std::runtime_error("Input out of range");
    }
}

void Calculator::writeOutput()
{
    if (!dataStack.empty())
    {
        auto topValue = popValueFromStack();
        if (std::holds_alternative<int>(topValue))
        {
            outputStream << std::get<int>(topValue) << std::endl;
        }
        else if (std::holds_alternative<double>(topValue))
        {
            outputStream << std::get<double>(topValue) << std::endl;
        }
        else if (std::holds_alternative<std::string>(topValue))
        {
            outputStream << std::get<std::string>(topValue) << std::endl;
        }
        else
        {
            throw std::runtime_error("Invalid value for output");
        }
    }
    else
    {
        throw std::runtime_error("Data stack is empty");
    }
}

void Calculator::run_test(const std::string &input)
{
    commandStream = input;
    while (!commandStream.empty())
    {
        char inputChar = commandStream[0];
        commandStream = commandStream.substr(1);

        if (operationMode > 0)
        {
            performStringConstructionMode(inputChar);
        }
        else if (operationMode == -1)
        {
            performIntegerConstructionMode(inputChar);
        }
        else if (operationMode < -1)
        {
            performDecimalPlaceConstructionMode(inputChar);
        }
        else
        {
            executeCommand(inputChar);
        }
        printEverything();
    }

    // Save state of stack
    std::string ts;
    std::stringstream ss(ts);
    printStack(ss, false, false);
    outStack = ss.str();
}

void Calculator::run()
{
    // get string from register a
    if (registerSet.find('a') != registerSet.end())
    {
        if (std::holds_alternative<std::string>(registerSet['a']))
            commandStream = std::get<std::string>(registerSet['a']);
        else if (std::holds_alternative<int>(registerSet['a']))
            commandStream = std::to_string(std::get<int>(registerSet['a']));
        else if (std::holds_alternative<double>(registerSet['a']))
            commandStream = std::to_string(std::get<double>(registerSet['a']));
    }
    
    while (true)
    {
        if (commandStream.empty())
        {
            std::cout << "Command stream is empty, please enter new command" << std::endl;
            getline(std::cin, commandStream);
        }

        char inputChar = commandStream[0];
        commandStream = commandStream.substr(1);

        if (operationMode > 0)
        {
            performStringConstructionMode(inputChar);
        }
        else if (operationMode == -1)
        {
            performIntegerConstructionMode(inputChar);
        }
        else if (operationMode < -1)
        {
            performDecimalPlaceConstructionMode(inputChar);
        }
        else
        {
            executeCommand(inputChar);
        }
        if(printEnabled)
        printEverything();
    }

    // Save state of stack
    std::string ts;
    std::stringstream ss(ts);
    printStack(ss, false, false);
    outStack = ss.str();
}