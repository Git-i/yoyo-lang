#include "parser.h"
#include <iostream>
#include <precedences.h>
#include "statement.h"
#include "func_sig.h"
namespace Yoyo
{
    Parser::Parser(std::string src) : source(std::move(src)), scn(source)
    {
        auto prefix_op_parselet = std::make_shared<PrefixOperationParselet>();
        auto name_parselet = std::make_shared<NameParselet>();
        auto bool_parselet = std::make_shared<BoolLiteralParselet>();
        auto lambda_parselet = std::make_shared<LambdaParselet>();
        prefixParselets[TokenType::Minus] = prefix_op_parselet;
        prefixParselets[TokenType::Bang] = prefix_op_parselet;
        prefixParselets[TokenType::Tilde] = prefix_op_parselet;
        prefixParselets[TokenType::IntegerLiteral] = std::make_shared<IntLiteralParselet>();
        prefixParselets[TokenType::RealLiteral] = std::make_shared<RealLiteralParselet>();
        prefixParselets[TokenType::StringLiteral] = std::make_shared<StringLiteralParselet>();
        prefixParselets[TokenType::Identifier] = name_parselet;
        prefixParselets[TokenType::This] = name_parselet;
        prefixParselets[TokenType::SPIdentifier] = name_parselet;
        prefixParselets[TokenType::LParen] = std::make_shared<GroupParselet>();
        prefixParselets[TokenType::True] = bool_parselet;
        prefixParselets[TokenType::False] = bool_parselet;
        prefixParselets[TokenType::LSquare] = std::make_shared<ArrayLiteralParselet>();
        prefixParselets[TokenType::Pipe] = lambda_parselet;
        prefixParselets[TokenType::DoublePipe] = lambda_parselet;

        auto sum_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Sum);
        auto product_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Product);
        auto equality_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Equality);
        auto relational_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Relational);
        auto access_parselet = std::make_shared<BinaryOperationParselet>(Precedences::MemberAccess);
        infixParselets[TokenType::Plus] = sum_parselet;
        infixParselets[TokenType::Minus] = sum_parselet;
        infixParselets[TokenType::Star] = product_parselet;
        infixParselets[TokenType::Slash] = product_parselet;
        infixParselets[TokenType::Percent] = product_parselet;
        infixParselets[TokenType::DoubleEqual] = equality_parselet;
        infixParselets[TokenType::BangEqual] = equality_parselet;
        infixParselets[TokenType::Greater] = relational_parselet;
        infixParselets[TokenType::GreaterEqual] = relational_parselet;
        infixParselets[TokenType::LessEqual] = relational_parselet;
        infixParselets[TokenType::Less] = relational_parselet;
        infixParselets[TokenType::Dot] = access_parselet;

        infixParselets[TokenType::Pipe] = std::make_shared<BinaryOperationParselet>(Precedences::BitOr);
        infixParselets[TokenType::Ampersand] = std::make_shared<BinaryOperationParselet>(Precedences::BitAnd);
        infixParselets[TokenType::Caret] = std::make_shared<BinaryOperationParselet>(Precedences::BitXor);

        auto assign_parselet = std::make_shared<BinaryOperationParselet>(Precedences::Assignment, true);
        infixParselets[TokenType::Equal] = assign_parselet;
        infixParselets[TokenType::PlusEqual] = assign_parselet;
        infixParselets[TokenType::MinusEqual] = assign_parselet;
        infixParselets[TokenType::StarEqual] = assign_parselet;
        infixParselets[TokenType::SlashEqual] = assign_parselet;
        infixParselets[TokenType::PercentEqual] = assign_parselet;
        infixParselets[TokenType::AmpersandEqual] = assign_parselet;
        infixParselets[TokenType::PipeEqual] = assign_parselet;
        infixParselets[TokenType::CaretEqual] = assign_parselet;

        infixParselets[TokenType::DoubleAmpersand] = std::make_shared<LogicalOperationParselet>(Precedences::LogicAnd);
        infixParselets[TokenType::DoublePipe] = std::make_shared<LogicalOperationParselet>(Precedences::LogicOr);

        infixParselets[TokenType::Bang] = std::make_shared<PostfixOperationParselet>(Precedences::InvalidPropagate);
        infixParselets[TokenType::LParen] = std::make_shared<CallOperationParselet>(Precedences::Call);

        infixParselets[TokenType::DoubleColon] = std::make_shared<ScopeOperationParselet>(Precedences::ScopeResolve);
    }
    PrefixParselet* Parser::GetPrefixParselet(TokenType t)
    {
        if(const auto f = prefixParselets.find(t); f != prefixParselets.end())
            return f->second.get();
        return nullptr;
    }
    InfixParselet* Parser::GetInfixParselet(TokenType t)
    {
        if(const auto f = infixParselets.find(t); f != infixParselets.end())
            return f->second.get();
        return nullptr;
    }
    uint32_t Parser::GetNextPrecedence()
    {
        auto tk = Peek();
        if(!tk) return 0;
        auto parselet = GetInfixParselet(tk->type);
        if(!parselet) return 0;
        return parselet->precedence();
    }

    std::unique_ptr<Statement> Parser::parseModuleImport(Token identifier)
    {
        Get();
        if(!discard(TokenType::Equal)) error("Expected '='", Peek());
        std::string name(identifier.text);
        auto iden = Get();
        if(!iden) return nullptr;
        if(iden->type != TokenType::Identifier) error("Expected identifier", Peek());
        return std::make_unique<ModuleImport>(std::move(name), std::string{iden->text});
    }

    std::unique_ptr<Statement> Parser::parseTopLevelDeclaration()
    {
        auto iden = Get();
        if(!iden) return nullptr;
        if(iden->type != TokenType::Identifier) error("Expected Identifier", iden);
        Get();//discard the ':'
        auto look_ahead = Peek();
        if(!look_ahead) return nullptr;
        switch (look_ahead->type)
        {
        case TokenType::LParen: return parseFunctionDeclaration(iden.value());
        case TokenType::Class: return parseClassDeclaration(iden.value(), false);
        case TokenType::Struct: return parseClassDeclaration(iden.value(), true);
        case TokenType::Enum: return parseEnumDeclaration(iden.value());
        case TokenType::EnumFlag:;//TODO
        case TokenType::Union:;//TODO
        case TokenType::Module: return parseModuleImport(iden.value());

        default: return parseVariableDeclaration(iden.value());
        }
    }

    std::vector<std::unique_ptr<Statement>> Parser::parseProgram()
    {
        std::vector<std::unique_ptr<Statement>> statements;
        while(Peek() && Peek()->type != TokenType::Eof)
        {
            statements.push_back(parseTopLevelDeclaration());
        }
        return statements;
    }
    bool Parser::isTopLevelDeclaration()
    {
        auto tk = Peek();
        if(tk && tk->type == TokenType::Identifier)
        {
            Get();
            if(Peek() && Peek()->type == TokenType::Colon)
            {
                pushToken(*tk);
                return true;
            }
            pushToken(*tk);
            return false;
        }
        return false;
    }

    bool Parser::discard(TokenType t)
    {
        auto tk = Peek();
        if(tk && tk->type == t)
        {
            Get();
            return true;
        }
        return false;
    }

    void Parser::pushToken(Token t)
    {
        peekBuffer.push_back(t);
    }

    void Parser::error(std::string message, std::optional<Token> tk)
    {
        has_error = true;
        std::cerr << message << std::endl;
        std::cerr << "at token: " << (tk ? "Invalid" : tk->text) << std::endl;
    }

    void Parser::synchronizeTo(std::span<const TokenType> t)
    {
        while(Peek() && std::ranges::find(t, Peek()->type) == t.end()) Get();
    }

    std::unique_ptr<Statement> Parser::parseFunctionDeclaration(Token identifier)
    {
        auto sig = parseFunctionSignature();
        //functions must specify explicit return types
        if(sig->returnType.name == "__inferred") sig->returnType.name = "void";
        if(!sig) synchronizeTo({{TokenType::Equal}});
        if(!discard(TokenType::Equal))
        {
            error("Expected '='", Peek());
        }
        auto stat = parseStatement();
        return std::make_unique<FunctionDeclaration>(identifier, std::move(sig).value_or(FunctionSignature{}), std::move(stat));
    }

    std::optional<FunctionSignature> Parser::parseFunctionSignature()
    {
        if(!discard(TokenType::LParen)) return std::nullopt;
        FunctionSignature sig;
        auto parseParam = [this](std::string name) -> std::optional<FunctionParameter>
        {
            /*
             * fn_name: (_: in int, : inout float)
             *              ^         ^        <- This function starts here
             */
            auto tk = Peek();
            if(!tk) return std::nullopt;
            auto t = ParamType::In;
            if(tk->type == TokenType::In) Get();
            else if(tk->type == TokenType::InOut)
            {
                t = ParamType::InOut;
                Get();
            }
            //param is a function
            else if(tk->type == TokenType::Called)
            {
                Get();
                auto sig = parseFunctionSignature();
                auto signature = std::make_shared<FunctionSignature>(*sig);
                Type t{.name = "__called_fn", .subtypes = {}, .signature = signature, .is_mutable = false};
                return FunctionParameter{.type = t, .convention = ParamType::In, .name = std::move(name)};
            }
            auto type = parseType(0);
            return FunctionParameter{std::move(type).value_or(Type{}), t, std::move(name)};
        };
        auto tk = Peek();
        if(!tk) return std::nullopt;
        if(tk->type == TokenType::Underscore)
        {
            Get();
            if(!discard(TokenType::Colon)) error("Expected ':'", Peek());
            auto p = parseParam("");
            if(!p) synchronizeTo({{TokenType::Comma, TokenType::RParen}});
            sig.parameters.push_back(std::move(p).value_or(FunctionParameter{}));
        }
        else if(tk->type == TokenType::Colon)
        {
            Get();
            auto p = parseParam("");
            if(!p) synchronizeTo({{TokenType::Comma, TokenType::RParen}});
            sig.parameters.push_back(std::move(p).value_or(FunctionParameter{}));
        }
        else if(tk->type == TokenType::Identifier)
        {
            Get();
            if(!discard(TokenType::Colon)) error("Expected ':'", Peek());
            auto p = parseParam(std::string(tk->text));
            if(!p) synchronizeTo({{TokenType::Comma, TokenType::RParen}});
            sig.parameters.push_back(std::move(p).value_or(FunctionParameter{}));
        }
        //this is allowed for first parameter
        else if(tk->type == TokenType::This)
        {
            Get();
            auto conv = ParamType::In;
            if(discard(TokenType::Colon))
            {
                auto conv_token = Peek();
                if(!conv_token) return std::nullopt;
                if(conv_token->type == TokenType::In){Get(); conv = ParamType::In;}
                else if(conv_token->type == TokenType::InOut){Get();conv = ParamType::InOut;}
                else error("Expected 'in' or 'inout'", conv_token);
            }
            sig.parameters.push_back(FunctionParameter{Type{"This", {}}, conv, "this"});
        }
        while(discard(TokenType::Comma))
        {
            auto tk = Peek();
            if(!tk) return std::nullopt;
            if(tk->type == TokenType::Underscore)
            {
                Get();
                if(!discard(TokenType::Colon)) error("Expected ':'", Peek());
                auto p = parseParam("");
                if(!p) synchronizeTo({{TokenType::Comma, TokenType::RParen}});
                sig.parameters.push_back(std::move(p).value_or(FunctionParameter{}));
            }
            else if(tk->type == TokenType::Colon)
            {
                Get();
                auto p = parseParam("");
                if(!p) synchronizeTo({{TokenType::Comma, TokenType::RParen}});
                sig.parameters.push_back(std::move(p).value_or(FunctionParameter{}));
            }
            else if(tk->type == TokenType::Identifier)
            {
                Get();
                if(!discard(TokenType::Colon)) error("Expected ':'", Peek());
                auto p = parseParam(std::string(tk->text));
                if(!p) synchronizeTo({{TokenType::Comma, TokenType::RParen}});
                sig.parameters.push_back(std::move(p).value_or(FunctionParameter{}));
            }
        }
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        sig.return_is_ref = false;
        sig.returnType = Type{"__inferred", {}};
        if(discard(TokenType::Arrow))
        {
            auto ref_tk = Peek();
            if(ref_tk->type == TokenType::Ref)
            {
                Get();
                sig.return_is_ref = true;
            }
            auto ret_type = parseType(0);
            sig.returnType = std::move(ret_type).value_or(Type{});
        }
        return sig;
    }

    std::optional<Token> Parser::Peek()
    {
        if(!peekBuffer.empty())
        {
            return peekBuffer.back();
        }
        auto tk = scn.NextToken();
        if(!tk) return std::nullopt;
        peekBuffer.push_back(tk.value());
        return peekBuffer.back();
    }

    std::optional<Token> Parser::Get()
    {
        if(!peekBuffer.empty())
        {
            auto bk = peekBuffer.back();
            peekBuffer.pop_back();
            return bk;
        }
        return scn.NextToken();
    }

    std::unique_ptr<Expression> Parser::parseExpression(uint32_t precedence)
    {
        auto tk = Get();
        if(!tk) return nullptr; //todo: handle invalid token
        auto prefixParselet = GetPrefixParselet(tk->type);
        if(!prefixParselet)
        {
            error("Unexpected token", *tk);
            return nullptr;
        }
        auto left = prefixParselet->parse(*this, tk.value());

        while(precedence < GetNextPrecedence())
        {
            //GetNext ensures we have a valid token
            tk = Get();
            auto infixParselet = GetInfixParselet(tk->type);
            left = infixParselet->parse(*this, std::move(left), *tk);
        }
        return left;
    }

    std::unique_ptr<Statement> Parser::parseVariableDeclaration(Token identifier)
    {
        bool is_mut = discard(TokenType::Mut);
        auto tk = Peek();
        if(!tk) return nullptr;
        if(tk->type == TokenType::Underscore)
        {
            Get();
            if(!discard(TokenType::Equal)) error("Expected '='", Peek());
            auto initializer = parseExpression(0);
            if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
            return std::make_unique<VariableDeclaration>(identifier, std::nullopt, std::move(initializer), is_mut);
        }
        if(tk->type == TokenType::Equal)
        {
            Get();
            auto initializer = parseExpression(0);
            //';' is implicit if a variable is initialized with a lambda
            if(!discard(TokenType::SemiColon) && !dynamic_cast<LambdaExpression*>(initializer.get()))
                error("Expected ';'", Peek());
            return std::make_unique<VariableDeclaration>(identifier, std::nullopt, std::move(initializer), is_mut);
        }
        auto type = parseType(0);
        if(!type) synchronizeTo({{TokenType::Equal, TokenType::SemiColon}});
        std::unique_ptr<Expression> init = nullptr;
        if(discard(TokenType::Equal))
        {
            init = parseExpression(0);
        }
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return std::make_unique<VariableDeclaration>(identifier, type, std::move(init), is_mut);
    }

    std::unique_ptr<Statement> Parser::parseEnumDeclaration(Token identifier)
    {
        //Unspecified values cannot equal to specified values unlike in c++
        std::vector<int32_t> usedIntegers;
        std::unordered_map<std::string, int32_t> values;
        int32_t nextValue = 0;
        Get(); //skip "enum"
        if(!discard(TokenType::Equal)) error("Expected '='", Peek());
        if(!discard(TokenType::LCurly)) error("Expected '{'", Peek());
        while(!discard(TokenType::RCurly))
        {
            auto name_tk = Get();
            if(name_tk->type != TokenType::Identifier) { error("Expected identifier", Peek()); continue; }
            std::string name(name_tk->text);
            if(values.contains(name)) error("Duplicate enum names", Peek());

            int32_t value = nextValue;
            bool is_explicit_value = false;
            //explicit value specified
            if(discard(TokenType::Equal))
            {
                auto value_tk = Peek();
                if(value_tk->type != TokenType::IntegerLiteral)
                {
                    error("Expected integer", Peek());
                    synchronizeTo({{TokenType::Comma, TokenType::RCurly}});
                }
                else
                {
                    Get();
                    is_explicit_value = true;
                    value = std::stoi(std::string{value_tk->text});
                    if(std::ranges::find(usedIntegers, value) != usedIntegers.end()) error("Duplicate enum values", Peek());
                }
            }
            if(is_explicit_value)
            {
                auto pos = std::ranges::find(usedIntegers, value);
                while(pos != usedIntegers.end())
                    pos = std::ranges::find(usedIntegers, ++value);
            }
            nextValue = value + 1;
            values[name] = value;
            if(!discard(TokenType::Comma))
            {
                if(!discard(TokenType::RCurly)) error("Expected ',' or '}'", Peek());
                else break;
            }
        }
        return std::make_unique<EnumDeclaration>(identifier, std::move(values));
    }
    //struct and classes are virtually the same, except structs dont have access specs and cant implement interfaces
    std::unique_ptr<Statement> Parser::parseClassDeclaration(Token identifier, bool isStruct)
    {
        std::vector<ClassMethod> methods;
        std::vector<ClassVariable> vars;
        Get(); //skip the "class" or "struct" keyword
        if(!discard(TokenType::Equal)) error("Expected '='", Peek());
        if(!discard(TokenType::LCurly)) error("Expected '{'", Peek());
        while(!discard(TokenType::RCurly))
        {
            bool is_static = false;
            AccessSpecifier spec = isStruct ? AccessSpecifier::Public : AccessSpecifier::Private;
            while(true)
            {
                auto tk = Peek();
                if(!tk) return nullptr;
                if(tk->type == TokenType::Static)
                {
                    if(is_static) error("Multiple 'static' on one object", tk);
                    is_static = true;
                    Get();
                }
                else if(tk->type == TokenType::Pub)
                {
                    //pub is redundant for structs but allowed
                    if(spec != AccessSpecifier::Private) error("Multiple access specifiers on one object", tk);
                    spec = AccessSpecifier::Public;
                    Get();
                }
                else if(tk->type == TokenType::Mod)
                {
                    if(isStruct) error("Cannot specify module-level access for a struct", tk);
                    if(spec != AccessSpecifier::Private) error("Multiple access specifiers on one object", tk);
                    spec = AccessSpecifier::Module;
                    Get();
                }
                else if(tk->type == TokenType::Identifier) break;
                else error("Unexpected token, expected '_', 'pub', 'mod' or identifier", Peek());
            }
            auto iden = *Get();
            if(!discard(TokenType::Colon)) error("Expected ':'", Peek());
            auto next_tk = Peek();
            if(!next_tk) return nullptr;
            if(next_tk->type == TokenType::LParen)
            {
                if(is_static) error("'static' cannot be applied to methods", next_tk);//static doesn't apply to functions
                auto stat = parseFunctionDeclaration(iden);
                methods.push_back(ClassMethod{.name=std::string{iden.text}, .function_decl = std::move(stat), .access = spec});
                std::ignore = discard(TokenType::Comma); //comma is optional after function
            }
            else
            {
                auto type = parseType(0);
                vars.push_back(ClassVariable{.access = spec,
                    .name = std::string{iden.text},
                    .type = std::move(type).value_or(Type{}),
                    .is_static = is_static});
                if(!discard(TokenType::Comma))
                {
                    if(!discard(TokenType::RCurly)) error("Expected ',' or '}'", Peek());
                    else break;
                }
            }
        }
        return std::make_unique<ClassDeclaration>(identifier, std::move(vars), std::move(methods));
    }
    //For this fn, the type has already been discarded
    /// Syntax:
    /// {.name1 = value1, .name2 = value2}
    std::unordered_map<std::string, std::unique_ptr<Expression>> Parser::parseObjectLiteral()
    {
        Get(); //discard '{'
        if(discard(TokenType::RCurly)) return {}; //empty initialization, should this be allowed!??
        std::unordered_map<std::string, std::unique_ptr<Expression>> result;
        while(!discard(TokenType::RCurly))
        {
            if(!discard(TokenType::Dot)) error("Expected '.'", Peek());
            auto iden = Peek();
            if(!iden) error("Invalid Token", iden);
            if(iden->type != TokenType::Identifier) error("Expected identifier", iden);
            std::string name(iden->text);
            if(result.contains(name)) error("Duplicate initialization of " + name, iden);
            Get(); //get the identifier
            if(!discard(TokenType::Equal)) error("Expected '='", Peek());
            result[name] = parseExpression(0);
            //must be a comma or '}' and trailing commas are allowed
            if(!discard(TokenType::Comma))
            {
                if(!discard(TokenType::RCurly)) error("Expected ',' or '}'", Peek());
                else break;
            }
        }
        return result;
    }
    std::unique_ptr<Statement> Parser::parseDeclaration()
    {
        if(isTopLevelDeclaration())
        {
            return parseTopLevelDeclaration();
        }
        return parseStatement();
    }

    std::unique_ptr<Statement> Parser::parseReturnStatement()
    {
        //empty return
        if(Peek() && Peek()->type == TokenType::SemiColon)
        {
            Get();
            return std::make_unique<ReturnStatement>(nullptr);
        }
        auto expr = parseExpression(0);
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return std::make_unique<ReturnStatement>(std::move(expr));
    }

    std::unique_ptr<Statement> Parser::parseExpressionStatement()
    {
        auto expr = parseExpression(0);
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return std::make_unique<ExpressionStatement>(std::move(expr));
    }

    std::unique_ptr<Statement> Parser::parseIfStatement()
    {
        if(!discard(TokenType::LParen)) error("Expected '('", Peek());
        auto condition = parseExpression(0);
        if(!condition) synchronizeTo({{TokenType::RParen}});
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        auto then = parseStatement();

        std::unique_ptr<Statement> else_stat = nullptr;
        auto else_tk = Peek();
        if(else_tk && else_tk->type == TokenType::Else)
        {
            Get();
            else_stat = parseStatement();
        }
        return std::make_unique<IfStatement>(std::move(condition), std::move(then), std::move(else_stat));
    }
    std::unique_ptr<Statement> Parser::parseBlockStatement()
    {
        std::vector<std::unique_ptr<Statement>> statements;
        while(!discard(TokenType::RCurly))
        {
            auto decl = parseDeclaration();
            statements.push_back(std::move(decl));
        }
        return std::make_unique<BlockStatement>(std::move(statements));
    }
    std::unique_ptr<Statement> Parser::parseForStatement()
    {
        if(!discard(TokenType::LParen)) error("Expected '('", Peek());
        std::vector<Token> vars;
        auto iden = Peek();
        if(!iden) return nullptr;
        if(iden->type != TokenType::Identifier || iden->type != TokenType::Underscore) error("Expected identifier or '_'", Peek());
        Get();
        vars.emplace_back(*iden);
        while(discard(TokenType::Comma))
        {
            iden = Peek();
            if(!iden) return nullptr;
            if(iden->type != TokenType::Identifier || iden->type != TokenType::Underscore) error("Expected identifier or '_'", Peek());
            Get();
            vars.emplace_back(*iden);
        }
        if(!discard(TokenType::In)) error("Expected 'in'", Peek());
        auto expr = parseExpression(0);
        if(!expr) synchronizeTo({{TokenType::RParen}});
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        auto then = parseStatement();
        return std::make_unique<ForStatement>(std::move(vars), std::move(expr), std::move(then));
    }

    std::unique_ptr<Statement> Parser::parseWhileStatement()
    {
        if(!discard(TokenType::LParen)) error("Expected '('", Peek());
        auto condition = parseExpression(0);
        if(!condition) synchronizeTo({{TokenType::RParen}});
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        auto body = parseStatement();
        return std::make_unique<WhileStatement>(std::move(condition), std::move(body));
    }
    std::unique_ptr<Statement> Parser::parseStatement()
    {
        auto tk = Peek();
        if(!tk) return nullptr;
        switch(tk->type)
        {
        case TokenType::Return: {Get(); return parseReturnStatement();}
        case TokenType::If: {Get(); return parseIfStatement();}
        case TokenType::LCurly: {Get(); return parseBlockStatement();}
        case TokenType::While: {Get(); return parseWhileStatement();}
        case TokenType::For: {Get(); return parseForStatement();}
        default: return parseExpressionStatement();
        }

    }

    std::optional<Type> parseArrayType(Token t, Parser& parser)
    {
        auto next = parser.parseType(0);
        if(!next) parser.synchronizeTo({{TokenType::RSquare}});
        if(!parser.discard(TokenType::RSquare)) parser.error("Expected ']'", parser.Peek());
        return Type("__arr", {std::move(next).value_or(Type{})});
    }
    std::optional<Type> parseTypeGroup(Token t, Parser& parser)
    {
        auto next = parser.parseType(0);
        if(!next) parser.synchronizeTo({{TokenType::RCurly}});
        if(!parser.discard(TokenType::RCurly)) parser.error("Expected '}'", parser.Peek());
        return std::move(next).value_or(Type{});
    }
    static constexpr uint32_t PipePrecedence = 1;
    static constexpr uint32_t AmpersandPrecedence = 2;
    static constexpr uint32_t TemplatePrecedence = 3;
    static constexpr uint32_t ScopePrecedence = 4;
    uint32_t Parser::GetNextTypePrecedence()
    {
        auto tk = Peek();
        if(!tk) return 0;
        switch(tk->type)
        {
        case TokenType::Ampersand: return AmpersandPrecedence;
        case TokenType::Pipe: return PipePrecedence;
        case TokenType::TemplateOpen: return TemplatePrecedence;
        case TokenType::DoubleColon: return ScopePrecedence;
        default: return 0;
        }
    }
    std::optional<Type> parseAmpTypeExpr(Parser& p, Type left)
    {
        auto t = p.parseType(AmpersandPrecedence);
        if(left.name == "__tup")
        {
            left.subtypes.push_back(std::move(t).value_or(Type{}));
            return left;
        }
        return Type("__tup", {left, std::move(t).value_or(Type{})});
    }
    std::optional<Type> parsePipeTypeExpr(Parser& p, Type left)
    {
        auto t = p.parseType(PipePrecedence);
        if(left.name == "__var")
        {
            left.subtypes.push_back(std::move(t).value_or(Type{}));
            return left;
        }
        return Type("__var", {left, std::move(t).value_or(Type{})});
    }
    std::optional<Type> parseTemplateTypeExpr(Parser& p, Type left)
    {
        auto t = p.parseType(0);
        if(!t) p.synchronizeTo({{TokenType::Comma, TokenType::Greater, TokenType::DoubleGreater}});
        left.subtypes.push_back(std::move(t).value_or(Type{}));
        while(p.discard(TokenType::Comma))
        {
            t = p.parseType(0);
            if(!t) p.synchronizeTo({{TokenType::Comma, TokenType::Greater, TokenType::DoubleGreater}});
            left.subtypes.push_back(std::move(t).value_or(Type{}));
        }
        if(!p.discard(TokenType::Greater))
        {
            if(p.discard(TokenType::DoubleGreater))
            {
                p.pushToken(Token{TokenType::Greater});
                return left;
            }
            p.error("Expected '>'", p.Peek());
        }
        return left;
    }
    std::optional<Type> Parser::parseType(uint32_t precedence)
    {
        auto tk = Peek();
        if(!tk) return std::nullopt;
        std::optional<Type> t;
        switch(tk->type)
        {
        case TokenType::Identifier: Get(); t = Type(std::string(tk->text), {}); break;
        case TokenType::LSquare: Get(); t = parseArrayType(*tk, *this); break;
        case TokenType::LCurly: Get(); t = parseTypeGroup(*tk, *this); break;
        default: t = std::nullopt;
        }
        while(precedence < GetNextTypePrecedence())
        {
            tk = Get();
            switch(tk->type)
            {
            case TokenType::Ampersand: t = parseAmpTypeExpr(*this, std::move(t).value()); break;
            case TokenType::Pipe: t = parsePipeTypeExpr(*this, std::move(t).value()); break;
            case TokenType::TemplateOpen: t = parseTemplateTypeExpr(*this, std::move(t).value()); break;
            case TokenType::DoubleColon:
                {
                    auto tp = parseType(precedence);
                    tp->name = t->name + "::" + tp->name;
                    t = std::move(tp);
                }
            default: /*unreachable*/;
            }
        }
        return t;
    }
}
