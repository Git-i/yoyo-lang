#pragma once

#include "type.h"
namespace Yoyo
{
    struct Attribute;
    class Statement;
    enum class AccessSpecifier
    {
        Public, ///< Every entity can access
        Private, ///< Only friends can access
        Module ///< Every entity within namespace can access
        //No protected because no inheritance
    };
    struct ClassVariable
    {
        AccessSpecifier access;
        std::string name;
        Type type;
        bool is_static;
        std::vector<Attribute> attributes;
    };
    struct ClassMethod
    {
        std::string name;
        std::unique_ptr<Statement> function_decl;
        AccessSpecifier access;
    };

}