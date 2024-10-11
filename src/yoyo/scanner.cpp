#include "scanner.h"

namespace Yoyo {
    Scanner::Scanner(std::istream& stream) : source(stream) {}
    Token Scanner::NextToken()
    {
        char c = source.get();
        switch (c)
        {
        case '+': return {TokenType::Plus};
        case '-': return {TokenType::Minus};
        case '*': return {TokenType::Star};
        }
    }
}