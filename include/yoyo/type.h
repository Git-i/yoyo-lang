#pragma once
#include <string>
#include <vector>

namespace Yoyo
{
    class Type
    {
    public:
        std::string name;
        std::vector<Type> subtypes;
        Type(std::string name, std::vector<Type> subtypes) : name(std::move(name)), subtypes(std::move(subtypes)) {};
        Type() = default;
    };
}
