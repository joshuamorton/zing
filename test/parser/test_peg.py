import unittest
import zing.parser.peg as peg


arithmetic = """
Expr     <- Factor AddExpr*
AddExpr  <- ('+'/'-') Factor
Factor   <- Primary MulExpr*
MulExpr  <- ('*'/'/') Primary
Primary  <- '(' Expr ')' / Number
Number   <- '0' / [1-9] [0-9_]
"""

PEG_GRAMMAR = r"""
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

class TestPEGMetaParser(unittest.TestCase):
    """
    These tests are only looking at the API
    """

    def setUp(self):
        self.p = peg.P

    def test_empty(self):
        with self.assertRaisesRegex(peg.TerminalError, "Expected a identifier at ''"):
            self.p("")

    def test_one_defn_ast(self):
        r = self.p("A <- B")
        self.assertEqual(r.functions.keys(), set(["A"]))
        self.assertEqual(len(r.syntax_tree), 3)
        self.assertEqual(len(r.syntax_tree[1]), 5)
        self.assertEqual(r.syntax_tree[1][2].string, "<-")

    

class TestMetaParserInternals(unittest.TestCase):
    """
    Tests of internal methods, to help with any debugging
    """

    terminals = {"LEFTARROW": "<-", "SLASH": "/", "AND": "&", "NOT": "!",
                 "QUESTION": "?", "STAR": "*", "PLUS": "+", "OPEN": "(",
                 "CLOSE": ")", "DOT": "."}
    raw_terminals = {"LEFTARROW": r"<-", "SLASH": r"/", "AND": r"&",
                 "NOT": r"!", "QUESTION": r"\?", "STAR": r"\*", "PLUS": r"\+",
                 "OPEN": r"\(", "CLOSE": r"\)", "DOT": r"\."}

    def setUp(self):
        # create the parser object without calling its init method, since
        # we are testing various parts of the init method, and we don't want
        # *all* of our tests to break during initialization if anything breaks
        self.p = peg.P.__new__(peg.P)
    
    def test_all_terminals(self):
        for s, t in self.terminals.items():
            n = self.p.__getattribute__("_"+s)(t)
            self.assertEqual(t, n[0][0].string)

    def test_terminal_function(self):
        for s, t in self.raw_terminals.items():
            n = self.p._terminal(t, s)(self.terminals[s])
            self.assertEqual(self.terminals[s], n[0][0].string)

    def test_literal(self):
        double = self.p._literal('"Hello World"')
        single = self.p._literal("'Hello World'")

        self.assertEqual(double[0][0].string, '"Hello World"')
        self.assertEqual(double[0][0].type, "literal")
        self.assertEqual(single[0][0].string, "'Hello World'")
        self.assertEqual(single[0][0].type, "literal")

        # make sure none of these throw errors
        self.p._literal('"\n"')

    def test_suffix(self):
        pass
