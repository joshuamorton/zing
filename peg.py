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


class P:
    def __init__(self, grammar):
        self.grammar = grammar
        self.syntax_tree, _ = self._grammar(grammar)
        print(self.syntax_tree)


    @staticmethod
    def _grammar(grammar):
        """
        all _x are of the form str -> ([Node], str)

        Grammar <- Spacing Definition+ EndOfFile

        """

        gram = Node("grammar")
        spacing, rest = P._spacing(grammar)
        children = [spacing]

        definitions, rest = P._some(P._definition)(rest)
        children += definitions
        pprint.pprint(children)
        eof, rest = P._EOF(rest)
        children += eof
        gram.children = children

        return [gram], rest  # rest will be empty here

    @staticmethod
    def _definition(rest):
        """
        Definition <- Identifier LEFTARROW Expression
        """
        defn = Node("definition")
        ident, rest = P._IDENTIFIER(rest)
        arrow, rest = P._LEFTARROW(rest)
        exp, rest = P._expression(rest)
        defn.children = ident + arrow + exp
        return [defn], rest

    @staticmethod
    def _expression(rest):
        """
        Expression <- Sequence (SLASH Sequence)*
        """
        expr = Node("expression")
        seq, rest = P._sequence(rest)
        nodes, rest = P._maybe(P._some(P._paren(P._SLASH, P._sequence)))(rest)

        expr.children = seq + nodes
        return [expr], rest

    @staticmethod
    def _sequence(rest):
        """
        Sequence <- Prefix*
        """
        seq = Node("sequence")
        nodes, rest = P._maybe(P._some(P._prefix))(rest)
        seq.children = nodes
        return [seq], rest


    @staticmethod
    def _prefix(rest):
        """
        Prefix <- (AND / NOT)? Suffix
        """
        prefix = Node("prefix")
        nodes, rest = P._maybe(P._slashed(P._AND, P._NOT))(rest)
        suffix, rest = P._suffix(rest)
        prefix.children = nodes + suffix
        return [prefix], rest

    @staticmethod
    def _suffix(rest):
        """
        Suffix <- Primary (QUESTION / STAR / PLUS)?
        """
        suffix = Node("suffix")
        prim, rest = P._primary(rest)
        nodes, rest = P._maybe(P._slashed(P._QUESTION, P._STAR, P._PLUS))(rest)
        suffix.children = prim + nodes
        return [suffix], rest


    @staticmethod
    def _primary(rest):
        """
        Primary <- Identifier (!LEFTARROW) / (OPEN Expression CLOSE) / Literal / Class / DOT
        """
        prim = Node("primary")
        nodes, rest = P._slashed(P._paren(P._IDENTIFIER, P._not(P._LEFTARROW)), P._paren(P._OPEN, P._expression,P._CLOSE), P._literal, P._class, P._DOT)(rest)
        prim.children = nodes
        return [prim], rest


    @staticmethod
    def _IDENTIFIER(rest):
        """
        Identifier <- IdentStart IdentCont* Spacing
        IdentStart <- [a-zA-Z_]
        IdentCont <- IdentStart / [0-9]
        """
        return P._terminal(r'[a-zA-Z_][a-zA-Z0-9_]*', "identifier")(rest)

    @staticmethod
    def _literal(rest):
        """
        Literal <- [''] (!['] Char)* [''] Spacing / ["] (!["] Char)* ["] Spacing
        """
        try:
            if rest[0] == "'":
                return P._terminal(r"""\'(.|\n|\r|\r\n)*?\'""", "literal")(rest)
            else:
                return P._terminal(r"""\"(.|\n|\r|\r\n)*\?\"""", "literal")(rest)
        except:
            raise GrammarError
        

    @staticmethod
    def _class(rest):
        """
        Class <- '[' (!']' Range)* ']' Spacing
        """
        return P._terminal(r'\[(.(-.)?)*\]', "range")(rest)


    @staticmethod
    def _terminal(terminal, name):
        def inner(rest):
            try:
                pos = re.match(terminal, rest).end()
                node = [Terminal(name, rest[:pos])]
                rest = rest[pos:]
            except:
                raise TerminalError

            spacing, rest = P._spacing(rest)
            return [node] + spacing, rest
        return inner

    @staticmethod
    def _LEFTARROW(rest):
        """
        LEFTARROW <- '<-' Spacing
        """
        return P._terminal(r'<-', "LEFTARROW")(rest)

    @staticmethod
    def _SLASH(rest):
        """
        SLASH <- '/' Spacing
        """
        return P._terminal(r'/', "SLASH")(rest)

    @staticmethod
    def _AND(rest):
        """
        AND <- '&' Spacing
        """
        return P._terminal(r'&', "AND")(rest)

    @staticmethod
    def _NOT(rest):
        """
        NOT <- '!' Spacing
        """
        return P._terminal(r'!', "NOT")(rest)

    @staticmethod
    def _QUESTION(rest):
        """
        QUESTION <- '?' Spacing
        """
        return P._terminal(r'\?', "QUESTION")(rest)

    @staticmethod
    def _STAR(rest):
        """
        STAR <- '*' Spacing
        """
        return P._terminal(r'\*', "STAR")(rest)

    @staticmethod
    def _PLUS(rest):
        """
        PLUS <- '+' Spacing
        """
        return P._terminal(r'\+', "PLUS")(rest)

    @staticmethod
    def _OPEN(rest):
        """
        OPEN <- '(' Spacing
        """
        return P._terminal(r'\(', "OPEN")(rest)

    @staticmethod
    def _CLOSE(rest):
        """
        CLOSE <- ')' Spacing
        """
        return P._terminal(r'\)', "CLOSE")(rest)

    @staticmethod
    def _DOT(rest):
        """
        DOT <- '.' Spacing
        """
        return P._terminal(r'\.', "DOT")(rest)

    @staticmethod
    def _spacing(rest):
        """
        Spacing <- (Space / Comment)*
        """
        spacing = Node("spacing")
        nodes, rest = P._maybe(P._some(P._paren(P._slashed(P._SPACE, P._COMMENT))))(rest)
        spacing.children = nodes
        return [spacing], rest

    @staticmethod
    def _COMMENT(rest):
        try:
            pos = re.match(r"#.*?(\r\n|\n|\r|$)", rest).end()
            return [Terminal("comment", rest[:pos])], rest[pos:]
        except:
            raise TerminalError

    @staticmethod
    def _SPACE(rest):
        try:
            pos = re.match(r"( |\t|\r\n|\n|\r)+", rest).end()
            return [Terminal("space", rest[:pos])], rest[pos:]
        except:
            raise TerminalError


    @staticmethod
    def _EOF(rest):
        if rest != "":
            raise TerminalError
        else:
            return [Terminal("eof", None)], None

    @staticmethod
    def _some(parser):
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
    def _maybe(parser):
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
    def _paren(*parsers):
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
    def _slashed(*parsers):
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
    def _not(parser):
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

p1 = P(gram)







