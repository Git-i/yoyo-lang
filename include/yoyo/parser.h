#pragma once
#include <scanner.h>
#include <span>
#include <statement.h>
#include <type.h>
#include <unordered_map>

#include "parselets/prefix_parselets.h"
#include "parselets/infix_parselets.h"
#include <vector>
namespace Yoyo
{
    class YOYO_API Parser
    {
    public:
        explicit Parser(std::string& source);
        std::unique_ptr<ASTNode> parseExpressionOrDeclaration();
        bool canOmitSemiColon(Expression*);
        std::unique_ptr<Expression> parseExpression(uint32_t precedence);
        std::unique_ptr<Statement> parseVariableDeclaration(Token identifier);
        std::unique_ptr<Statement> parseEnumDeclaration(Token identifier);
        std::unique_ptr<Statement> parseConstDeclaration(Token identifier);
        std::unique_ptr<Statement> parseClassDeclaration(Token identifier, bool);
        std::unique_ptr<Statement> parseAliasDeclaration(Token identifier);
        std::unique_ptr<Statement> parseInterfaceDeclaration(Token identifier);
        std::unique_ptr<Statement> parseUnionDeclaration(Token identifier);
        std::unique_ptr<Statement> parseMacroDeclaration(Token identifier);

        std::unique_ptr<Statement> parseUsingDeclaration(Token);
        std::unordered_map<std::string, std::unique_ptr<Expression>> parseObjectLiteral();
        std::unique_ptr<Statement> parseDeclaration();
        std::unique_ptr<Statement> parseReturnStatement(Token);
        std::unique_ptr<Statement> parseExpressionStatement();
        std::unique_ptr<Statement> parseConditionalExtraction(Token);
        std::unique_ptr<Statement> parseForStatement(Token);
        std::unique_ptr<Statement> parseWhileStatement(Token);
        std::unique_ptr<Statement> parseStatement();
        std::unique_ptr<Statement> parseWithStatement(Token);
        std::unique_ptr<Statement> parseBreakContinueStatement(Token tk);
        uint32_t GetNextTypePrecedence();
        std::vector<std::unique_ptr<Statement>> parseProgram();
        std::optional<Type> parseType(uint32_t precedence);
        // I'm being experimental with using syntax to support nexted generics
        // so one can write:
        // using Type::Type2::<?1> as Alias::<?1>;
        // using Type::<?1>::{ Type2::<?2>::Type3::<?3> as Alias1::<?1, ?2, ?3>, Type2Alt::<?2> as AliasAlt::<?1, ?2> }
        // so the number trackes what generic we're on
        UsingStatement::ContentTy parseUsingContent(uint32_t);
        [[nodiscard]] bool discard(TokenType t);
        [[nodiscard]] bool failed() const {return has_error;};
        void pushToken(Token t, SourceLocation loc);
        void error(std::string message, std::optional<Token> tk);

        void synchronizeTo(std::span<const TokenType> t);///< skip all tokens till the next token of type @c t
        std::optional<Token> Peek();
        std::optional<Token> Get();
        std::optional<std::pair<Token, SourceLocation>> PeekWithEndLocation();
        std::optional<std::pair<Token, SourceLocation>> GetWithEndLocation();
        std::optional<FunctionSignature> parseFunctionSignature();
        std::optional<GenericClause> parseGenericClause();
        std::vector<Attribute> parseAttributeList();
        const Scanner& getScanner() const { return scn; };
        const std::string& getSource() const { return source; };
        ASTNode* parent = nullptr;
        SourceLocation discardLocation;
    private:
        friend class MacroExprEval;
        bool has_error = false;
        std::unique_ptr<Statement> parseFunctionDeclaration(Token identifier);

        uint32_t GetNextPrecedence();
        std::unique_ptr<Statement> parseModuleImport(Token identifier);
        std::unique_ptr<Statement> parseTopLevelDeclaration();
        std::unique_ptr<Statement> parseOperatorOverload(const Token& tok);
        bool isTopLevelDeclaration();
        bool isVarDeclaration();
        PrefixParselet* GetPrefixParselet(TokenType t);
        InfixParselet* GetInfixParselet(TokenType t);
        std::string& source;
        Scanner scn;
        std::unordered_map<TokenType, std::shared_ptr<PrefixParselet>> prefixParselets;
        std::unordered_map<TokenType, std::shared_ptr<InfixParselet>> infixParselets;
        std::vector<std::pair<Token, SourceLocation>> peekBuffer;
    };
}