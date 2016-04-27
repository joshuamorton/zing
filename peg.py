import pprint
import re
gram = """
Grammar <- Spacing Definition+ EndOfFile
Definition <- Identifier LEFTARROW Expression
Expression <- Sequence (SLASH Sequence)*
Sequence <- Prefix*
Prefix <- (AND / NOT)? Suffix
Suffix <- Primary (QUESTION / STAR / PLUS)?
Primary <- Identifier !LEFTARROW
/ OPEN Expression CLOSE
/ Literal / Class / DOT
# Lexical syntax
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / [0-9]
Literal <- [''] (![''] Char)* [''] Spacing
/ ["] (!["] Char)* ["] Spacing
Class <- '[' (!']' Range)* ']' Spacing
Range <- Char '-' Char / Char
Char <- '\\' [nrt'"\[\]\\]
/ '\\' [0-2][0-7][0-7]
/ '\\' [0-7][0-7]?
/ !'\\' .
LEFTARROW <- '<-' Spacing
SLASH <- '/' Spacing
AND <- '&' Spacing
NOT <- '!' Spacing
QUESTION <- '?' Spacing
STAR <- '*' Spacing
PLUS <- '+' Spacing
OPEN <- '(' Spacing
CLOSE <- ')' Spacing
DOT <- '.' Spacing
Spacing <- (Space / Comment)*
Comment <- '#' (!EndOfLine .)* EndOfLine
Space <- ' ' / '\t' / EndOfLine
EndOfLine <- '\r\n' / '\n' / '\r'
EndOfFile <- !.
"""


arithmetic = """
Expr     <- Factor AddExpr*
AddExpr  <- ('+'/'-') Factor
Factor   <- Primary MulExpr*
MulExpr  <- ('*'/'/') Primary
Primary  <- '(' Expr ')'
         / Number
Number   <- '0' / [1-9] ([0-9] / _)+
"""
class Node:
    def __init__(self, t):
        self.type = t
        self._string = None
        self.children = None

    @property
    def string(self):
        return None  # "complex" recursive thing
    
    def __repr__(self):
        return str(self.children)

class Terminal(Node):
    def __init__(self, t, string):
        self.type = t
        self._string = string
        self.children = None

    @property
    def string(self):
        return self._string

    def __repr__(self):
        if self.type == "space":
            return ""
        return self.string

class GrammarError(Exception):
    pass

class TerminalError(GrammarError):
    pass

class SlashError(GrammarError):
    pass

class NotError(GrammarError):
    pass


class Parser:
    def __init__(self, grammar):
        self.grammar = grammar
        self.parsed_grammar = self._parse_grammar(grammar)

    @staticmethod
    def _parse_grammar(grammar):
        """
        all _parse_x are of the form str -> ([Node], str)

        Grammar <- Spacing Definition+ EndOfFile

        """

        gram = Node("grammar")
        spacing, rest = Parser._parse_spacing(grammar)
        children = [spacing]

        definitions, rest = Parser._parse_some(Parser._parse_definition)(rest)
        children += definitions
        pprint.pprint(children)
        eof, rest = Parser._parse_EOF(rest)
        children += eof
        gram.children = children

        return [gram], rest  # rest will be empty here

    @staticmethod
    def _parse_definition(rest):
        """
        Definition <- Identifier LEFTARROW Expression
        """
        defn = Node("definition")
        ident, rest = Parser._parse_IDENTIFIER(rest)
        arrow, rest = Parser._parse_LEFTARROW(rest)
        exp, rest = Parser._parse_expression(rest)
        defn.children = ident + arrow + exp
        return [defn], rest

    @staticmethod
    def _parse_expression(rest):
        """
        Expression <- Sequence (SLASH Sequence)*
        """
        expr = Node("expression")
        seq, rest = Parser._parse_sequence(rest)
        nodes, rest = Parser._parse_maybe(Parser._parse_some(Parser._parse_paren(
                                                        Parser._parse_SLASH,
                                                        Parser._parse_sequence)))(rest)

        expr.children = seq + nodes
        return [expr], rest

    @staticmethod
    def _parse_sequence(rest):
        """
        Sequence <- Prefix*
        """
        seq = Node("sequence")
        nodes, rest = Parser._parse_maybe(Parser._parse_some(Parser._parse_prefix))(rest)
        seq.children = nodes
        return [seq], rest


    @staticmethod
    def _parse_prefix(rest):
        """
        Prefix <- (AND / NOT)? Suffix
        """
        prefix = Node("prefix")
        nodes, rest = Parser._parse_maybe(Parser._parse_slashed(Parser._parse_AND, Parser._parse_NOT))(rest)
        suffix, rest = Parser._parse_suffix(rest)
        prefix.children = nodes + suffix
        return [prefix], rest

    @staticmethod
    def _parse_suffix(rest):
        """
        Suffix <- Primary (QUESTION / STAR / PLUS)?
        """
        suffix = Node("suffix")
        prim, rest = Parser._parse_primary(rest)
        nodes, rest = Parser._parse_maybe(Parser._parse_slashed(Parser._parse_QUESTION,
                                                                Parser._parse_STAR,
                                                                Parser._parse_PLUS))(rest)
        suffix.children = prim + nodes
        return [suffix], rest


    @staticmethod
    def _parse_primary(rest):
        """
        Primary <- Identifier (!LEFTARROW) / (OPEN Expression CLOSE) / Literal / Class / DOT
        """
        prim = Node("primary")
        nodes, rest = Parser._parse_slashed(Parser._parse_paren(Parser._parse_IDENTIFIER, Parser._parse_not(Parser._parse_LEFTARROW)),
                                            Parser._parse_paren(Parser._parse_OPEN, Parser._parse_expression,Parser._parse_CLOSE),
                                            Parser._parse_literal,
                                            Parser._parse_class,
                                            Parser._parse_DOT)(rest)
        prim.children = nodes
        return [prim], rest


    @staticmethod
    def _parse_IDENTIFIER(rest):
        """
        Identifier <- IdentStart IdentCont* Spacing
        IdentStart <- [a-zA-Z_]
        IdentCont <- IdentStart / [0-9]
        """
        return Parser._parse_terminal(r'[a-zA-Z_][a-zA-Z0-9_]*', "identifier")(rest)

    @staticmethod
    def _parse_literal(rest):
        """
        Literal <- [''] (!['] Char)* [''] Spacing / ["] (!["] Char)* ["] Spacing
        """
        try:
            if rest[0] == "'":
                return Parser._parse_terminal(r"""\'(.|\n|\r|\r\n)*?\'""", "literal")(rest)
            else:
                return Parser._parse_terminal(r"""\"(.|\n|\r|\r\n)*\?\"""", "literal")(rest)
        except:
            raise GrammarError
        

    @staticmethod
    def _parse_class(rest):
        """
        Class <- '[' (!']' Range)* ']' Spacing
        """
        return Parser._parse_terminal(r'\[(.(-.)?)*\]', "range")(rest)


    @staticmethod
    def _parse_terminal(terminal, name):
        def inner(rest):
            try:
                pos = re.match(terminal, rest).end()
                node = [Terminal(name, rest[:pos])]
                rest = rest[pos:]
            except:
                raise TerminalError

            spacing, rest = Parser._parse_spacing(rest)
            return [node] + spacing, rest
        return inner

    @staticmethod
    def _parse_LEFTARROW(rest):
        """
        LEFTARROW <- '<-' Spacing
        """
        return Parser._parse_terminal(r'<-', "LEFTARROW")(rest)

    @staticmethod
    def _parse_SLASH(rest):
        """
        SLASH <- '/' Spacing
        """
        return Parser._parse_terminal(r'/', "SLASH")(rest)

    @staticmethod
    def _parse_AND(rest):
        """
        AND <- '&' Spacing
        """
        return Parser._parse_terminal(r'&', "AND")(rest)

    @staticmethod
    def _parse_NOT(rest):
        """
        NOT <- '!' Spacing
        """
        return Parser._parse_terminal(r'!', "NOT")(rest)

    @staticmethod
    def _parse_QUESTION(rest):
        """
        QUESTION <- '?' Spacing
        """
        return Parser._parse_terminal(r'\?', "QUESTION")(rest)

    @staticmethod
    def _parse_STAR(rest):
        """
        STAR <- '*' Spacing
        """
        return Parser._parse_terminal(r'\*', "STAR")(rest)

    @staticmethod
    def _parse_PLUS(rest):
        """
        PLUS <- '+' Spacing
        """
        return Parser._parse_terminal(r'\+', "PLUS")(rest)

    @staticmethod
    def _parse_OPEN(rest):
        """
        OPEN <- '(' Spacing
        """
        return Parser._parse_terminal(r'\(', "OPEN")(rest)

    @staticmethod
    def _parse_CLOSE(rest):
        """
        CLOSE <- ')' Spacing
        """
        return Parser._parse_terminal(r'\)', "CLOSE")(rest)

    @staticmethod
    def _parse_DOT(rest):
        """
        DOT <- '.' Spacing
        """
        return Parser._parse_terminal(r'\.', "DOT")(rest)

    @staticmethod
    def _parse_spacing(rest):
        """
        Spacing <- (Space / Comment)*
        """
        spacing = Node("spacing")
        nodes, rest = Parser._parse_maybe(Parser._parse_some(Parser._parse_paren(
            Parser._parse_slashed(Parser._parse_SPACE, 
                                  Parser._parse_COMMENT))))(rest)
        spacing.children = nodes
        return [spacing], rest

    @staticmethod
    def _parse_COMMENT(rest):
        try:
            pos = re.match(r"#.*?(\r\n|\n|\r|$)", rest).end()
            return [Terminal("comment", rest[:pos])], rest[pos:]
        except:
            raise TerminalError

    @staticmethod
    def _parse_SPACE(rest):
        try:
            pos = re.match(r"( |\t|\r\n|\n|\r)+", rest).end()
            return [Terminal("space", rest[:pos])], rest[pos:]
        except:
            raise TerminalError


    @staticmethod
    def _parse_EOF(rest):
        if rest != "":
            raise TerminalError
        else:
            return [Terminal("eof", None)], None

    @staticmethod
    def _parse_some(parser):
        """
        parses at least one of the passed in parser
        """
        def inner(rest):
            node, rest = parser(rest)
            nodes = [node]
            while True:
                try:
                    node, rest = parser(rest)
                    nodes += node
                except GrammarError as e:
                    break
            return nodes, rest
        return inner

    @staticmethod
    def _parse_maybe(parser):
        """
        parses an optional item
        """
        def inner(rest):
            try:
                node, rest = parser(rest)
            except GrammarError as e:
                node, rest = [], rest
            return [node], rest
        return inner

    @staticmethod
    def _parse_paren(*parsers):
        """
        parses a parenthetical
        """
        def inner(rest):
            nodes = []
            for parser in parsers:
                node, rest = parser(rest)
                nodes += node
            return nodes, rest
        return inner

    @staticmethod
    def _parse_slashed(*parsers):
        """
        parses slash seperated values
        """
        def inner(rest):
            for parser in parsers:
                try:
                    node, rest = parser(rest)
                    return node, rest
                except GrammarError as e:
                    pass

            raise SlashError
        return inner

    @staticmethod
    def _parse_not(parser):
        """
        parses a not lookahead
        """
        def inner(rest):
            try:
                parser(rest)
            except GrammarError:
                return [], rest
            raise GrammarError
        return inner
#p1 = Parser(gram)

p = Parser(gram)








