#include "parser.h"
#include <iostream>
#include <precedences.h>
#include "statement.h"
#include "func_sig.h"
namespace Yoyo
{
    Parser::Parser(std::string& src) : source(src), scn(source)
    {
        auto prefix_op_parselet = std::make_shared<PrefixOperationParselet>();
        auto name_parselet = std::make_shared<NameParselet>();
        auto bool_parselet = std::make_shared<BoolLiteralParselet>();
        auto lambda_parselet = std::make_shared<LambdaParselet>();
        prefixParselets[TokenType::Minus] = prefix_op_parselet;
        prefixParselets[TokenType::Bang] = prefix_op_parselet;
        prefixParselets[TokenType::Tilde] = prefix_op_parselet;
        prefixParselets[TokenType::Star] = prefix_op_parselet;
        prefixParselets[TokenType::Ampersand] = prefix_op_parselet;
        prefixParselets[TokenType::IntegerLiteral] = std::make_shared<IntLiteralParselet>();
        prefixParselets[TokenType::RealLiteral] = std::make_shared<RealLiteralParselet>();
        prefixParselets[TokenType::StringLiteral] = std::make_shared<StringLiteralParselet>();
        prefixParselets[TokenType::CharLiteral] = std::make_shared<CharLiteralParselet>();
        prefixParselets[TokenType::Identifier] = name_parselet;
        prefixParselets[TokenType::This] = name_parselet;
        prefixParselets[TokenType::SPIdentifier] = name_parselet;
        prefixParselets[TokenType::LParen] = std::make_shared<GroupParselet>();
        prefixParselets[TokenType::True] = bool_parselet;
        prefixParselets[TokenType::False] = bool_parselet;
        prefixParselets[TokenType::LSquare] = std::make_shared<ArrayLiteralParselet>();
        prefixParselets[TokenType::Pipe] = lambda_parselet;
        prefixParselets[TokenType::DoublePipe] = lambda_parselet;
        prefixParselets[TokenType::Null] = std::make_shared<NullLiteralParselet>();

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
        infixParselets[TokenType::As] = std::make_shared<AsExpressionParselet>(Precedences::As);

        infixParselets[TokenType::DoubleAmpersand] = std::make_shared<LogicalOperationParselet>(Precedences::LogicAnd);
        infixParselets[TokenType::DoublePipe] = std::make_shared<LogicalOperationParselet>(Precedences::LogicOr);

        infixParselets[TokenType::Bang] = std::make_shared<PostfixOperationParselet>(Precedences::InvalidPropagate);
        infixParselets[TokenType::LParen] = std::make_shared<CallOperationParselet>(Precedences::Call);

        infixParselets[TokenType::DoubleColon] = std::make_shared<ScopeOperationParselet>(Precedences::ScopeResolve);

        infixParselets[TokenType::LSquare] = std::make_shared<SubscriptParselet>(Precedences::Call);
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
        auto attrs = parseAttributeList();
        auto iden = Get();
        if(!iden) return nullptr;
        if(iden->type == TokenType::Operator) return parseOperatorOverload(iden.value());
        if(iden->type != TokenType::Identifier) error("Expected Identifier", iden);
        Get();//discard the ':'
        auto look_ahead = Peek();
        if(!look_ahead) return nullptr;
        std::unique_ptr<Statement> decl;
        switch (look_ahead->type)
        {
        case TokenType::Fn: decl = parseFunctionDeclaration(iden.value()); break;
        case TokenType::Class: decl = parseClassDeclaration(iden.value(), false); break;
        case TokenType::Struct: decl = parseClassDeclaration(iden.value(), true); break;
        case TokenType::Enum: decl = parseEnumDeclaration(iden.value()); break;
        case TokenType::EnumFlag: break;//TODO
        case TokenType::Union: break;//TODO
        case TokenType::Alias: decl = parseAliasDeclaration(iden.value()); break;
        case TokenType::Module: decl = parseModuleImport(iden.value()); break;

        default: return parseVariableDeclaration(iden.value());
        }
        decl->attributes = std::move(attrs);
        return decl;
    }
    std::unique_ptr<Statement> Parser::parseOperatorOverload(const Token& tok)
    {
        if(!discard(TokenType::Colon)) error("Expected ':'", Peek());
        auto op = Get();
        if(!op->can_be_overloaded()) error("Operator cannot be overloaded", Peek());
        auto sig = parseFunctionSignature();
        if(sig->returnType.name == "__inferred") sig->returnType.name = "void";
        if(!discard(TokenType::Equal)) error("Expected '='", Peek());
        auto body = parseStatement();
        auto return_val = std::make_unique<OperatorOverload>(op->type, std::move(sig).value_or(FunctionSignature{}), std::move(body));
        return_val->body->parent = return_val.get();
        return Statement::attachSLAndParent(std::move(return_val), tok.loc, return_val->body->end, parent);
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
        if(!tk) return false;
        if(tk->type == TokenType::Operator) return true;
        if(tk->type == TokenType::Identifier)
        {
            auto [tok, loc] = *GetWithEndLocation();
            if(Peek() && Peek()->type == TokenType::Colon)
            {
                pushToken(tok, loc);
                return true;
            }
            pushToken(tok, loc);
            return false;
        }
        return false;
    }
    bool Parser::discard(TokenType t)
    {
        auto tk = Peek();
        if(tk && tk->type == t)
        {
            auto [nonsense, sl] = *GetWithEndLocation();
            discardLocation = sl;
            return true;
        }
        return false;
    }

    void Parser::pushToken(Token t, SourceLocation loc)
    {
        peekBuffer.emplace_back(t, loc);
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
        if(!discard(TokenType::Fn)) error("Expected 'fn'", Peek());
        auto next_tok = Peek();
        if(!next_tok) return nullptr;
        std::optional<GenericClause> gclause = next_tok->type == TokenType::TemplateOpen ?
            parseGenericClause() : std::nullopt;
        auto sig = parseFunctionSignature();
        //functions must specify explicit return types
        if(sig->returnType.name == "__inferred") sig->returnType.name = "void";
        if(!sig) synchronizeTo({{TokenType::Equal}});
        if(!discard(TokenType::Equal))
        {
            error("Expected '='", Peek());
        }
        auto stat = parseStatement();
        auto stat_ptr = stat.get();
        std::unique_ptr<Statement> fn_decl;
        if(gclause)
            fn_decl = std::make_unique<GenericFunctionDeclaration>(*std::move(gclause),
                FunctionDeclaration(std::string{identifier.text}, *std::move(sig), std::move(stat)));
        else
            fn_decl = std::make_unique<FunctionDeclaration>(std::string{identifier.text}, *std::move(sig), std::move(stat));
        stat_ptr->parent = fn_decl.get();
        return Statement::attachSLAndParent(std::move(fn_decl), identifier.loc, stat_ptr->end, parent);
    }
    Attribute parseAttribute(Parser& p)
    {
        Attribute attr;
        auto name = p.Get();
        if(!name) p.error("Invalid token", name);
        attr.name = name->text;
        if(p.discard(TokenType::LParen))
        {
            auto next_tk = p.Get();
            if(!next_tk) p.error("Invalid token", next_tk);
            if(next_tk->type != TokenType::Identifier && next_tk->type != TokenType::StringLiteral)
                p.error("Expected string", next_tk);
            attr.params.emplace_back(next_tk->text);
            while(!p.discard(TokenType::RParen))
            {
                next_tk = p.Get();
                if(!next_tk) p.error("Invalid token", next_tk);
                if(next_tk->type != TokenType::Identifier && next_tk->type != TokenType::StringLiteral)
                    p.error("Expected string", next_tk);
                attr.params.emplace_back(next_tk->text);
                if(!p.discard(TokenType::Comma))
                {
                    if(!p.discard(TokenType::RParen))
                        p.error("Expected ',' or ')'", p.Peek());
                    else break;
                }
            }
        }
        return attr;
    }
    std::vector<Attribute> Parser::parseAttributeList()
    {
        if(!discard(TokenType::AttrOpen)) return {};
        std::vector<Attribute> attr;
        attr.push_back(parseAttribute(*this));
        while(!discard(TokenType::RParen))
        {
            attr.push_back(parseAttribute(*this));
            if(!discard(TokenType::Comma))
            {
                if(!discard(TokenType::RParen))
                    error("Expected ',' or ')'", Peek());
                else break;
            }
        }
        return attr;
    }
    std::optional<FunctionSignature> Parser::parseFunctionSignature()
    {
        if(discard(TokenType::Arrow))
            return FunctionSignature{.returnType = *parseType(0), .return_is_ref = false, .parameters = {}};
        //empty signature
        if(!discard(TokenType::LParen))
        {
            return FunctionSignature{.returnType = {"__inferred"}, .return_is_ref = false, .parameters = {}};
        }
        FunctionSignature sig;
        auto parseParam = [this](std::string name) -> std::optional<FunctionParameter>
        {
            /*
             * fn_name: (_: in int, : inout float)
             *              ^         ^        <- This function starts here
             */
            auto tk = Peek();
            if(!tk) return std::nullopt;
            //param is a function
            else if(tk->type == TokenType::Called)
            {
                Get();
                auto sig = parseFunctionSignature();
                auto signature = std::make_shared<FunctionSignature>(*sig);
                Type t{.name = "__called_fn", .subtypes = {}, .signature = signature, .is_mutable = false};
                return FunctionParameter{.type = t, .name = std::move(name)};
            }
            auto type = parseType(0);
            return FunctionParameter{std::move(type).value_or(Type{}),  std::move(name)};
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
            sig.parameters.emplace_back(Type{"This", {}},  "this");
        }
        //&this and &mut this is also allowed
        else if(tk->type == TokenType::Ampersand)
        {
            Get(); bool is_mut = discard(TokenType::Mut);
            if(!discard(TokenType::This)) error("Expected 'this' after '&' or '&mut'", Peek());
            sig.parameters.emplace_back(Type{
                is_mut ? "__ref_mut" : "__ref",
                { Type{"This"}}
            }, "this");
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
            auto ret_type = parseType(0);
            sig.returnType = std::move(ret_type).value_or(Type{});
        }
        return sig;
    }
    // ::<a, b, c>
    std::optional<GenericClause> Parser::parseGenericClause()
    {
        std::vector<std::string> names;
        if(!discard(TokenType::TemplateOpen)) error("Expected '::<'", Peek());
        auto iden = Get();
        if(!iden) return std::nullopt;
        if(iden->type != TokenType::Identifier) error("Expected identifier", Peek());
        else names.emplace_back(iden->text);
        while(!discard(TokenType::Greater))
        {
            if(!discard(TokenType::Comma)) error("Expected ','", Peek());
            iden = Get();
            if(!iden) return std::nullopt;
            if(iden->type != TokenType::Identifier) error("Expected identifier", Peek());
            else names.emplace_back(iden->text);
        }
        return GenericClause(std::move(names));
    }

    std::optional<Token> Parser::Peek()
    {
        if(!peekBuffer.empty())
        {
            return peekBuffer.back().first;
        }
        auto tk = scn.NextToken();
        if(!tk) return std::nullopt;
        peekBuffer.emplace_back(tk.value(), scn.GetSourceLocation());
        return peekBuffer.back().first;
    }

    std::optional<Token> Parser::Get()
    {
        if(!peekBuffer.empty())
        {
            auto bk = peekBuffer.back();
            peekBuffer.pop_back();
            return bk.first;
        }
        return scn.NextToken();
    }

    std::optional<std::pair<Token, SourceLocation>> Parser::PeekWithEndLocation()
    {
        Peek();
        return peekBuffer.back();
    }

    std::optional<std::pair<Token, SourceLocation>> Parser::GetWithEndLocation()
    {
        auto bk = PeekWithEndLocation();
        peekBuffer.pop_back();
        return bk;
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
            return Statement::attachSLAndParent(
            std::make_unique<VariableDeclaration>(identifier, std::nullopt, std::move(initializer), is_mut),
            identifier.loc, discardLocation, parent);
        }
        if(tk->type == TokenType::Equal)
        {
            Get();
            auto initializer = parseExpression(0);
            //';' is implicit if a variable is initialized with a lambda
            if(!discard(TokenType::SemiColon) && !dynamic_cast<LambdaExpression*>(initializer.get()))
                error("Expected ';'", Peek());
            return Statement::attachSLAndParent(
                std::make_unique<VariableDeclaration>(identifier, std::nullopt, std::move(initializer), is_mut),
                identifier.loc, discardLocation, parent);
        }
        auto type = parseType(0);
        if(!type) synchronizeTo({{TokenType::Equal, TokenType::SemiColon}});
        std::unique_ptr<Expression> init = nullptr;
        if(discard(TokenType::Equal))
        {
            init = parseExpression(0);
        }
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return Statement::attachSLAndParent(
            std::make_unique<VariableDeclaration>(identifier, type, std::move(init), is_mut),
            identifier.loc, discardLocation, parent);
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
        return Statement::attachSLAndParent(
            std::make_unique<EnumDeclaration>(identifier, std::move(values)), identifier.loc, discardLocation, parent);
    }
    //struct and classes are virtually the same, except structs don't have access specs and cant implement interfaces
    std::unique_ptr<Statement> Parser::parseClassDeclaration(Token identifier, bool isStruct)
    {
        std::vector<ClassMethod> methods;
        std::vector<ClassVariable> vars;
        Get(); //skip the "class" or "struct" keyword
        Ownership own_method = Ownership::Owning;
        if(discard(TokenType::Colon))
        {
            if(!discard(TokenType::Ampersand)) error("Expected '&'", Peek());
            if(discard(TokenType::Mut)) own_method = Ownership::NonOwningMut;
            else own_method = Ownership::NonOwning;
        }
        if(!discard(TokenType::Equal)) error("Expected '='", Peek());
        if(!discard(TokenType::LCurly)) error("Expected '{'", Peek());
        while(!discard(TokenType::RCurly))
        {
            auto attr_list = parseAttributeList();
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
            if(next_tk->type == TokenType::Fn)
            {
                if(is_static) error("'static' cannot be applied to methods", next_tk);//static doesn't apply to functions
                auto stat = parseFunctionDeclaration(iden);
                stat->attributes = std::move(attr_list);
                methods.push_back(ClassMethod{.name=std::string{iden.text}, .function_decl = std::move(stat), .access = spec});
                std::ignore = discard(TokenType::Comma); //comma is optional after function
            }
            else
            {
                auto type = parseType(0);
                vars.push_back(ClassVariable{.access = spec,
                    .name = std::string{iden.text},
                    .type = std::move(type).value_or(Type{}),
                    .is_static = is_static,
                    .attributes = std::move(attr_list)
                });
                if(!discard(TokenType::Comma))
                {
                    if(!discard(TokenType::RCurly)) error("Expected ',' or '}'", Peek());
                    else break;
                }
            }
        }
        return Statement::attachSLAndParent(
            std::make_unique<ClassDeclaration>(identifier, std::move(vars), std::move(methods), own_method), identifier.loc,
            discardLocation, parent
        );
    }

    std::unique_ptr<Statement> Parser::parseAliasDeclaration(Token identifier)
    {
        Get(); // skip the `alias`
        auto next_tok = Peek();
        std::optional<GenericClause> clause;
        if(!next_tok) { error("Unexpected EOF", next_tok); return nullptr; }
        if(next_tok->type == TokenType::TemplateOpen) clause = parseGenericClause();
        if(!discard(TokenType::Equal)) error("Expected '='", Peek());
        Type tp = parseType(0).value_or(Type{});
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return Statement::attachSLAndParent(clause ?
            std::make_unique<GenericAliasDeclaration>(std::string(identifier.text), std::move(tp), *std::move(clause))
            : std::make_unique<AliasDeclaration>(std::string(identifier.text), std::move(tp)), identifier.loc, discardLocation, parent
            );
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
    std::unique_ptr<Statement> Parser::parseReturnStatement(Token return_tok)
    {
        //empty return
        if(discard(TokenType::SemiColon))
        {
            return Statement::attachSLAndParent(
                std::make_unique<ReturnStatement>(nullptr), return_tok.loc, discardLocation, parent);
        }
        auto expr = parseExpression(0);
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return Statement::attachSLAndParent(
            std::make_unique<ReturnStatement>(std::move(expr)), return_tok.loc, discardLocation, parent);
    }

    std::unique_ptr<Statement> Parser::parseExpressionStatement()
    {
        auto expr = parseExpression(0);
        if(!discard(TokenType::SemiColon)) error("Expected ';'", Peek());
        return Statement::attachSLAndParent(std::make_unique<ExpressionStatement>(std::move(expr)), expr->beg,
            discardLocation, parent);
    }

    std::unique_ptr<Statement> Parser::parseConditionalExtraction(Token tk)
    {
        //parse the capture
        bool is_ref = false;
        bool else_is_ref = false;
        if(!discard(TokenType::Pipe)) error("Expected '|'", Peek());
        if(discard(TokenType::Ampersand)) is_ref = true;
        auto iden = Get();
        if(!iden) return nullptr;
        if(iden->type != TokenType::Identifier) error("Expected identifier", iden);
        std::string name(iden->text);
        if(!discard(TokenType::Pipe)) error("Expected '|'", Peek());

        if(!discard(TokenType::LParen)) error("Expected '('", Peek());
        auto condition = parseExpression(0);
        if(!condition) synchronizeTo({{TokenType::RParen}});
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        auto then = parseStatement();
        SourceLocation end = then->end;

        std::unique_ptr<Statement> else_stat = nullptr;
        std::string else_name;
        auto else_tk = Peek();
        if(else_tk && else_tk->type == TokenType::Else)
        {
            Get();
            if(discard(TokenType::Pipe))
            {
                if(discard(TokenType::Ampersand)) else_is_ref = true;
                auto else_iden = Peek();
                if(!else_iden) return nullptr;
                if(else_iden->type != TokenType::Identifier) error("Expected identifier", else_iden);
                if(!discard(TokenType::Pipe)) error("Expected '|'", Peek());
                else_name.assign(else_iden->text.begin(), else_iden->text.end());
            }
            else_stat = parseStatement();
            end = else_stat->end;
        }
        return Statement::attachSLAndParent(
            std::make_unique<ConditionalExtraction>(name, is_ref, std::move(condition), std::move(then), std::move(else_stat), else_name, else_is_ref),
            tk.loc, end, parent
        );
    }

    std::unique_ptr<Statement> Parser::parseIfStatement(Token tk)
    {
        if(Peek() && Peek()->type == TokenType::Pipe) return parseConditionalExtraction(tk);
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
        SourceLocation end = else_stat ? else_stat->end : then->end;
        return Statement::attachSLAndParent(
            std::make_unique<IfStatement>(std::move(condition), std::move(then), std::move(else_stat)),
            tk.loc, end, parent);
    }
    std::unique_ptr<Statement> Parser::parseBlockStatement(Token tk)
    {
        std::vector<std::unique_ptr<Statement>> statements;
        while(!discard(TokenType::RCurly))
        {
            auto decl = parseDeclaration();
            statements.push_back(std::move(decl));
        }
        return Statement::attachSLAndParent(
            std::make_unique<BlockStatement>(std::move(statements)), tk.loc, discardLocation, parent);
    }
    std::unique_ptr<Statement> Parser::parseForStatement(Token tk)
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
        return Statement::attachSLAndParent(
            std::make_unique<ForStatement>(std::move(vars), std::move(expr), std::move(then)),
            tk.loc, then->end, parent);
    }

    std::unique_ptr<Statement> Parser::parseWhileStatement(Token tk)
    {
        if(!discard(TokenType::LParen)) error("Expected '('", Peek());
        auto condition = parseExpression(0);
        if(!condition) synchronizeTo({{TokenType::RParen}});
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        auto body = parseStatement();
        return Statement::attachSLAndParent(
            std::make_unique<WhileStatement>(std::move(condition), std::move(body)), tk.loc,
            body->end, parent);
    }
    std::unique_ptr<Statement> Parser::parseStatement()
    {
        auto tk = Peek();
        if(!tk) return nullptr;
        switch(tk->type)
        {
        case TokenType::Return: {Get(); return parseReturnStatement(*tk);}
        case TokenType::If: {Get(); return parseIfStatement(*tk);}
        case TokenType::LCurly: {Get(); return parseBlockStatement(*tk);}
        case TokenType::While: {Get(); return parseWhileStatement(*tk);}
        case TokenType::For: {Get(); return parseForStatement(*tk);}
        case TokenType::With: {Get(); return parseWithStatement(*tk);}
        default: return parseExpressionStatement();
        }

    }

    std::unique_ptr<Statement> Parser::parseWithStatement(Token tk)
    {
        if(!discard(TokenType::LParen)) error("Expected '('", Peek());
        auto name = Get();
        if(!name || name->type != TokenType::Identifier) error("Expected identifier", Peek());
        if(!discard(TokenType::As)) error("Expected 'as'", Peek());
        auto expr = parseExpression(0);
        if(!discard(TokenType::RParen)) error("Expected ')'", Peek());
        auto stat = std::make_unique<WithStatement>(std::string(name->text), std::move(expr), parseStatement());
        stat->body->parent = stat.get();
        auto end = stat->body->end;
        return Statement::attachSLAndParent(std::move(stat), tk.loc, end, parent);
    }

    std::optional<Type> parseArrayType(Token t, Parser& parser)
    {
        auto next = parser.parseType(0);
        if(!next) parser.synchronizeTo({{TokenType::RSquare}});
        size_t numElems = 0;
        if(parser.discard(TokenType::SemiColon))
        {
            auto size = parser.Peek();
            if(!size || size->type != TokenType::IntegerLiteral) parser.error("Invalid array size", size);
            else { parser.Get(); numElems = std::stoi(std::string(size->text)); }
        }
        else if (parser.discard(TokenType::Colon))
        {
            if (!parser.discard(TokenType::Ampersand)) parser.error("Expected '{'", parser.Peek());
            bool is_mut = parser.discard(TokenType::Mut);
            if (!parser.discard(TokenType::RSquare)) parser.error("Expected ']'", parser.Peek());
            return Type(is_mut ? "__slice_mut" : "__slice", { std::move(next).value_or(Type{}) });
        }
        if(!parser.discard(TokenType::RSquare)) parser.error("Expected ']'", parser.Peek());
        std::string name = numElems == 0 ? "__arr_d" : "__arr_s" + std::to_string(numElems);
        return Type(name, { std::move(next).value_or(Type{}) });
    }
    std::optional<Type> parseTypeGroup(Token t, Parser& parser)
    {
        auto next = parser.parseType(0);
        if(!next) parser.synchronizeTo({{TokenType::RCurly}});
        if(!parser.discard(TokenType::RCurly)) parser.error("Expected '}'", parser.Peek());
        return std::move(next).value_or(Type{});
    }
    //TODO reconsider this table
    static constexpr uint32_t PipePrecedence = 1;
    static constexpr uint32_t OptionalPreference = 3;
    static constexpr uint32_t TemplatePrecedence = 4;
    static constexpr uint32_t ScopePrecedence = 5;
    uint32_t Parser::GetNextTypePrecedence()
    {
        auto tk = Peek();
        if(!tk) return 0;
        switch(tk->type)
        {
        case TokenType::TemplateOpen: return TemplatePrecedence;
        case TokenType::DoubleColon: return ScopePrecedence;
        case TokenType::Question: return OptionalPreference;
        default: return 0;
        }
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
                p.pushToken(Token{TokenType::Greater}, p.discardLocation);
                return left;
            }
            p.error("Expected '>'", p.Peek());
        }
        return left;
    }
    std::optional<Type> parsePostfixTypeExpr(Parser& p, Type left, Token t)
    {
        if(t.type == TokenType::Question) return Type{ .name = "__opt", .subtypes = {std::move(left)}};
        return std::nullopt;
    }
    std::optional<Type> parseRefType(Token tk, Parser& p)
    {
        auto is_mut = p.discard(TokenType::Mut);
        auto t = *p.parseType(0);
        if(t.is_reference()) p.error("Double reference not allowed", tk);
        if(is_mut) return Type{"__ref_mut", {std::move(t)}};
        return Type("__ref", {std::move(t)});
    }
    std::optional<Type> parseGroupType(Parser& p)
    {
        if(p.discard(TokenType::RParen)) return Type{"void"};
        std::optional<TokenType> seperator;
        std::vector<Type> subtypes;
        while(!p.discard(TokenType::RParen))
        {
            subtypes.push_back(p.parseType(0).value_or(Type{}));
            if(p.discard(TokenType::RParen)) break;
            if(!seperator)
            {
                auto tk = p.Peek();
                if(!tk) return std::nullopt;
                seperator = tk->type;
                if(tk->type != TokenType::Comma && tk->type != TokenType::Pipe)
                {
                    p.error("Expected ',' or '|'", tk);
                    seperator = TokenType::Comma;
                }
            }
            if(!p.discard(*seperator))
                p.error("Expected ','", p.Peek());
        }
        Type t {.module = nullptr};
        if(subtypes.size() == 1) t = std::move(subtypes[0]);
        else t.subtypes = std::move(subtypes);

        if(seperator && *seperator == TokenType::Pipe) t.name = "__var";
        else if(seperator && *seperator == TokenType::Comma) t.name = "__tup";

        return t;
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
        case TokenType::Ampersand: Get(); t = parseRefType(*tk, *this); break;
        case TokenType::LParen: Get(); t = parseGroupType(*this); break;
        default: t = std::nullopt;
        }
        while(precedence < GetNextTypePrecedence())
        {
            tk = Get();
            switch(tk->type)
            {
            case TokenType::TemplateOpen: t = parseTemplateTypeExpr(*this, std::move(t).value()); break;
            case TokenType::Question: t = parsePostfixTypeExpr(*this, std::move(t).value(), *tk); break;
            case TokenType::DoubleColon:
                {
                    auto tp = parseType(precedence);
                    tp->name = t->full_name() + "::" + tp->name;
                    t = std::move(tp);
                }
            default: /*unreachable*/;
            }
        }
        return t;
    }
}
