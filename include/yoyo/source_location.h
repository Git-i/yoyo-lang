#pragma once
#include <cstddef>
namespace Yoyo
{
    struct SourceLocation
    {
        size_t line = 0;
        size_t column = 0;
    };
}