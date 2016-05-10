import re


class Node:
    def __init__(self, t, parser):
        self.parser = parser
        self.type = t
        self._string = None
        self.children = []

    @property
    def string(self):
        return self.__repr__()  # "complex" recursive thing
    
    def __repr__(self):
        return self.type + ":" + str(self.children)

    def __getitem__(self, i):
        return self.children[i]

    def __len__(self):
        return len(self.children)

    def __iter__(self):
        return iter(self.children)

    def handle(self, vals):
        return self.__getattribute__(self.type)(self, vals)

class PEGNode(Node):
    def expression(self, node):
        for i in range(0, len(node), 2):
            self.handle(node(i))

    def seqence(self, node):
        print(node)

    def prefix(self, node):
        pass

    def suffix(self, node):
        pass


class Terminal(Node):
    def __init__(self, t, parser, string):
        super().__init__(t, parser)
        self.parser = parser
        self.type = t
        self._string = string
        self.children = None

    @property
    def string(self):
        return self._string

    def __repr__(self):
        if self.type == "space" or self.type == "eof":
            return ""
        return "Terminal:" + '"' + self.string + '"'

    def __str__(self):
        return self._string


class PEGTerminal(Terminal):
    pass

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
        self.functions = dict()
        self.grammar = grammar
        self.syntax_tree, _ = self._grammar(grammar)
        self.syntax_tree = self.syntax_tree[0]

    def parse(self, string):
        return self._parse(string, self.syntax_tree)

    def _parse(self, string, node):
        """
        recursively parse nodes from the syntax
        """
        print(node.type)




 
   # for bootstrapping the PEG parse tree
    # these methods each are in the form 
    # function(String a) -> Tuple[Subtring, Node]
    # where Substring is some substring of a, from an index x (can be 0)
    #     to the end
    # and Node is a Node object that essentially represents part of the AST of
    #     the parser itself

    def _grammar(self, grammar):
        """
        all _x are of the form str -> ([Node], str)

        Grammar <- Spacing Definition+ EndOfFile

        """

        gram = Node("grammar", self)
        spacing, rest = self._spacing(grammar)
        children = spacing

        definitions, rest = self._some(self._definition)(rest)
        children += definitions
        eof, rest = self._EOF(rest)
        children += eof
        gram.children = children

        return [gram], rest  # rest will be empty here

    def _definition(self, rest):
        """
        Definition <- Identifier LEFTARROW Expression
        """
        defn = Node("definition", self)
        ident, rest = self._IDENTIFIER(rest)
        arrow, rest = self._LEFTARROW(rest)
        exp, rest = self._expression(rest)
        defn.children = ident + arrow + exp
        self.functions[defn.children[0].string] = defn.children[4]
        return [defn], rest

    def _expression(self, rest):
        """
        Expression <- Sequence (SLASH Sequence)*
        """
        expr = Node("expression", self)
        seq, rest = self._sequence(rest)
        nodes, rest = self._maybe(self._some(self._paren(self._SLASH, self._sequence)))(rest)

        expr.children = seq + nodes
        return [expr], rest

    def _sequence(self, rest):
        """
        Sequence <- Prefix*
        """
        seq = Node("sequence", self)
        nodes, rest = self._maybe(self._some(self._prefix))(rest)
        seq.children = nodes
        return [seq], rest

    def _prefix(self, rest):
        """
        Prefix <- (AND / NOT)? Suffix
        """
        prefix = Node("prefix", self)
        nodes, rest = self._maybe(self._slashed(self._AND, self._NOT))(rest)
        suffix, rest = self._suffix(rest)
        prefix.children = nodes + suffix
        return [prefix], rest

    def _suffix(self, rest):
        """
        Suffix <- Primary (QUESTION / STAR / PLUS)?
        """
        suffix = Node("suffix", self)
        prim, rest = self._primary(rest)
        nodes, rest = self._maybe(self._slashed(self._QUESTION, self._STAR, self._PLUS))(rest)
        suffix.children = prim + nodes
        return [suffix], rest

    def _primary(self, rest):
        """
        Primary <- Identifier (!LEFTARROW) / (OPEN Expression CLOSE) / Literal / Class / DOT
        """
        prim = Node("primary", self)
        nodes, rest = self._slashed(self._paren(self._IDENTIFIER, self._not(self._LEFTARROW)), self._paren(self._OPEN, self._expression,self._CLOSE), self._literal, self._class, self._DOT)(rest)
        prim.children = nodes
        return [prim], rest

    def _IDENTIFIER(self, rest):
        """
        Identifier <- IdentStart IdentCont* Spacing
        IdentStart <- [a-zA-Z_]
        IdentCont <- IdentStart / [0-9]
        """
        return self._terminal(r'[a-zA-Z_][a-zA-Z0-9_]*', "identifier")(rest)

    def _literal(self, rest):
        """
        Literal <- ['] (!['] Char)* ['] Spacing / ["] (!["] Char)* ["] Spacing
        """
        try:
            if rest[0] == "'":
                return self._terminal(r"""\'([^']|\n|\r|\r\n)*?\'""", "literal")(rest)
            else:
                return self._terminal(r"""\"([^"]|\n|\r|\r\n)*?\"""", "literal")(rest)
        except:
            raise GrammarError
        
    def _class(self, rest):
        """
        Class <- '[' (!']' Range)* ']' Spacing
        """
        return self._terminal(r'\[(.(-.)?)*\]', "range")(rest)

    def _terminal(self, terminal, name):
        """
        terminal: the raw string to match
        name: the name of the node
        """
        def inner(rest):
            try:
                pos = re.match(terminal, rest).end()
                node = [Terminal(name, self, rest[:pos])]
                rest = rest[pos:]
            except:
                raise TerminalError("Expected a {} at '".format(name) + rest[:min(10, len(rest))] + "'")

            spacing, rest = self._spacing(rest)
            return node + spacing, rest
        return inner

    def _LEFTARROW(self, rest):
        """
        LEFTARROW <- '<-' Spacing
        """
        return self._terminal(r'<-', "LEFTARROW")(rest)

    def _SLASH(self, rest):
        """
        SLASH <- '/' Spacing
        """
        return self._terminal(r'/', "SLASH")(rest)

    def _AND(self, rest):
        """
        AND <- '&' Spacing
        """
        return self._terminal(r'&', "AND")(rest)

    def _NOT(self, rest):
        """
        NOT <- '!' Spacing
        """
        return self._terminal(r'!', "NOT")(rest)

    def _QUESTION(self, rest):
        """
        QUESTION <- '?' Spacing
        """
        return self._terminal(r'\?', "QUESTION")(rest)

    def _STAR(self, rest):
        """
        STAR <- '*' Spacing
        """
        return self._terminal(r'\*', "STAR")(rest)

    def _PLUS(self, rest):
        """
        PLUS <- '+' Spacing
        """
        return self._terminal(r'\+', "PLUS")(rest)

    def _OPEN(self, rest):
        """
        OPEN <- '(' Spacing
        """
        return self._terminal(r'\(', "OPEN")(rest)

    def _CLOSE(self, rest):
        """
        CLOSE <- ')' Spacing
        """
        return self._terminal(r'\)', "CLOSE")(rest)

    def _DOT(self, rest):
        """
        DOT <- '.' Spacing
        """
        return self._terminal(r'\.', "DOT")(rest)

    def _spacing(self, rest):
        """
        Spacing <- (Space / Comment)*
        """
        spacing = Node("spacing", self)
        nodes, rest = self._maybe(self._some(self._paren(self._slashed(self._SPACE, self._COMMENT))))(rest)
        spacing.children = nodes
        return [spacing], rest

    def _COMMENT(self, rest):
        try:
            pos = re.match(r"#.*?(\n|\r|\r\n|$)", rest).end()
            return [Terminal("comment", self, rest[:pos])], rest[pos:]
        except:
            raise TerminalError("Expected a comment at '" + rest[:min(10, len(rest))] + "'")

    def _SPACE(self, rest):
        try:
            pos = re.match(r"( |\t|\r\n|\n|\r)+", rest).end()
            return [Terminal("space", self, rest[:pos])], rest[pos:]
        except:
            raise TerminalError("Expected a space at '" + rest[:min(10, len(rest))] + "'")

    def _EOF(self, rest):
        if rest != "":
            raise TerminalError("Expected an end of file at '" + rest[:min(10, len(rest))] + "'")
        else:
            return [Terminal("eof", self, None)], None

    @staticmethod
    def _some(parser):
        """
        parses at least one of the passed in parser
        """
        def inner(rest):
            node, rest = parser(rest)
            nodes = node
            while True:
                try:
                    node, rest = parser(rest)
                    nodes += node
                except GrammarError:
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
            except GrammarError:
                node, rest = [], rest
            return node, rest
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
                except GrammarError:
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



