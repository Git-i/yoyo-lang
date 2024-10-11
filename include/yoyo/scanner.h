#include <istream>
#include "token.h"
namespace Yoyo {
    class Scanner {
        Scanner(std::istream&);
        Token NextToken(); 
    private:
        std::istream& source;
    };
}