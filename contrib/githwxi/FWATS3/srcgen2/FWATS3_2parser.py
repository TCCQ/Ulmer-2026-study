"""Parse FWATS3 source into the level-2 trees in FWATS3_2basics."""
from contextlib import contextmanager
from dataclasses import dataclass
import re
from typing import Iterable

import FWATS3_2basics as ast


class FWATS3SyntaxError(SyntaxError):
    """Invalid FWATS3 source, with standard filename/line/offset attributes."""


_KEYWORDS = set("lam fix if then else let in end val dec imp local true false".split())
_DECLARATIONS = {"val", "dec", "imp", "local"}
_COMPARISONS = {"<", ">", "<=", ">=", "==", "!="}
_TOKEN = re.compile(
    r'(?P<space>\s+)|(?P<comment>\#[^\r\n]*)'
    r'|(?P<identifier>[A-Za-z_][A-Za-z_0-9$]*)'
    r'|(?P<integer>[0-9]+)'
    r'|(?P<symbol>->|=>|<=|>=|==|!=|[()\[\]{},;:.@=+*/%<>-])'
)
_ESCAPES = {'"': '"', "\\": "\\", "n": "\n", "r": "\r", "t": "\t"}


@dataclass(frozen=True)
class _Token:
    kind: str
    value: str
    start: int


def _fnlist(values):
    result = ast.fnlist_nil()
    for value in reversed(values):
        result = ast.fnlist_cons(value, result)
    return result


class _Parser:
    def __init__(self, source: str, filename: str, static_vars: Iterable[str]):
        self.source = source
        self.filename = filename
        self.static_vars = set(static_vars)
        self.tokens = list(self.tokenize())
        self.index = 0

    def error(self, message, position=None):
        if position is None:
            position = self.current.start
        line = self.source.count("\n", 0, position) + 1
        beginning = self.source.rfind("\n", 0, position) + 1
        ending = self.source.find("\n", position)
        if ending < 0:
            ending = len(self.source)
        raise FWATS3SyntaxError(
            message,
            (self.filename, line, position - beginning + 1,
             self.source[beginning:ending]),
        )

    def tokenize(self):
        pos = 0
        while pos < len(self.source):
            if self.source[pos] == '"':
                start = pos
                pos += 1
                chars = []
                while pos < len(self.source) and self.source[pos] != '"':
                    char = self.source[pos]
                    if char in "\r\n":
                        self.error("Unescaped newline in string", pos)
                    if char == "\\":
                        pos += 1
                        if pos >= len(self.source):
                            self.error("Unterminated string escape", pos)
                        if self.source[pos] not in _ESCAPES:
                            self.error("Unknown string escape", pos)
                        char = _ESCAPES[self.source[pos]]
                    chars.append(char)
                    pos += 1
                if pos == len(self.source):
                    self.error("Unterminated string", start)
                yield _Token("string", "".join(chars), start)
                pos += 1
                continue
            match = _TOKEN.match(self.source, pos)
            if match is None:
                self.error(f"Unexpected character {self.source[pos]!r}", pos)
            kind = match.lastgroup
            value = match.group()
            if kind not in {"space", "comment"}:
                if kind == "symbol" or value in _KEYWORDS:
                    kind = value
                yield _Token(kind, value, pos)
            pos = match.end()
        yield _Token("EOF", "", pos)

    @property
    def current(self):
        return self.tokens[self.index]

    def take(self, kind):
        if self.current.kind == kind:
            token = self.current
            self.index += 1
            return token
        return None

    def expect(self, kind):
        token = self.take(kind)
        if token is None:
            self.error(f"Expected {kind!r}, found {self.current.value!r}")
        return token

    @contextmanager
    def scope(self, names):
        previous = self.static_vars
        self.static_vars = previous | set(names)
        try:
            yield
        finally:
            self.static_vars = previous

    def comma_list(self, opening, closing, item, allow_empty=True):
        self.expect(opening)
        values = []
        if self.current.kind == closing and allow_empty:
            self.expect(closing)
            return values
        values.append(item())
        while self.take(","):
            if self.current.kind == closing:
                break
            values.append(item())
        self.expect(closing)
        return values

    def parameters(self, opening, closing, allow_empty=True):
        def name():
            token = self.expect("identifier")
            if token.value in names:
                self.error("Duplicate static parameter", token.start)
            names.add(token.value)
            return token.value
        names = set()
        return self.comma_list(opening, closing, name, allow_empty)

    def static_args(self):
        return _fnlist(self.comma_list("[", "]", self.type))

    def parenthesized(self, item, tuple_class):
        self.expect("(")
        if self.take(")"):
            return tuple_class(ast.fnlist_nil())
        first = item()
        if not self.take(","):
            self.expect(")")
            return first
        values = [first]
        if self.current.kind != ")":
            values.append(item())
            while self.take(","):
                if self.current.kind == ")":
                    break
                values.append(item())
        self.expect(")")
        return tuple_class(_fnlist(values))

    def type(self):
        if self.current.kind == "(":
            result = self.parenthesized(self.type, ast.S2Etupl)
        else:
            name = self.expect("identifier").value
            if name in self.static_vars:
                if self.current.kind == "[":
                    self.error("Static variables cannot take static arguments")
                result = ast.S2Evar(name)
            else:
                args = self.static_args() if self.current.kind == "[" else ast.fnlist_nil()
                result = ast.S2Econ(name, args)
        if self.take("->"):
            result = ast.S2Efun(result, self.type())
        return result

    def declarations(self):
        values = []
        while self.current.kind in _DECLARATIONS:
            values.append(self.declaration())
            if not self.take(";"):
                if self.current.kind in _DECLARATIONS:
                    self.error("Expected ';' between declarations")
                break
        return _fnlist(values)

    def declaration(self):
        if self.take("val"):
            name = self.expect("identifier").value
            self.expect("=")
            return ast.D2Cbind(name, self.expression())
        if self.take("dec"):
            name = self.expect("identifier").value
            params = self.parameters("[", "]") if self.current.kind == "[" else []
            self.expect(":")
            with self.scope(params):
                signature = self.type()
            return ast.D2Cdecl(name, _fnlist(params), signature)
        if self.take("imp"):
            params = self.parameters("{", "}", False) if self.current.kind == "{" else []
            name = self.expect("identifier").value
            with self.scope(params):
                patterns = self.static_args() if self.current.kind == "[" else ast.fnlist_nil()
                self.expect("=")
                body = self.expression()
            return ast.D2Cimpl(name, body, _fnlist(params), patterns)
        self.expect("local")
        private = self.declarations()
        self.expect("in")
        exported = self.declarations()
        self.expect("end")
        return ast.D2Clocal(private, exported)

    def expression(self):
        if self.current.kind in {"lam", "fix"}:
            recursive = self.take("fix") is not None
            if recursive:
                function = self.expect("identifier").value
            else:
                self.expect("lam")
            self.expect("(")
            parameter = self.expect("identifier")
            if recursive and function == parameter.value:
                self.error("Fix name and parameter must differ", parameter.start)
            self.expect(":")
            argument_type = self.type()
            self.expect(")")
            if recursive:
                self.expect(":")
                result_type = self.type()
            self.expect("=>")
            body = self.expression()
            if recursive:
                return ast.D2Efix(function, parameter.value, argument_type, body, result_type)
            return ast.D2Elam(parameter.value, argument_type, body)
        if self.take("if"):
            condition = self.expression()
            self.expect("then")
            yes = self.expression()
            self.expect("else")
            return ast.D2Eif0(condition, yes, self.expression())
        if self.take("let"):
            declarations = self.declarations()
            body = ast.fnoptn_cons(self.expression()) if self.take("in") else ast.fnoptn_nil()
            self.expect("end")
            return ast.D2Elets(declarations, body)
        result = self.additive()
        if self.current.kind in _COMPARISONS:
            operator = self.current.kind
            self.index += 1
            result = ast.D2Eop2(operator, result, self.additive())
            if self.current.kind in _COMPARISONS:
                self.error("Comparisons cannot be chained")
        return result

    def binary(self, child, operators):
        result = child()
        while self.current.kind in operators:
            operator = self.current.kind
            self.index += 1
            result = ast.D2Eop2(operator, result, child())
        return result

    def additive(self):
        return self.binary(self.multiplicative, {"+", "-"})

    def multiplicative(self):
        return self.binary(self.unary, {"*", "/", "%"})

    def unary(self):
        if self.current.kind in {"+", "-"}:
            operator = self.current.kind
            self.index += 1
            return ast.D2Eop1(operator, self.unary())
        return self.postfix()

    def postfix(self):
        result = self.atom()
        while True:
            if self.current.kind == "(":
                result = ast.D2Eapp(result, self.parenthesized(self.expression, ast.D2Etupl))
            elif self.take("."):
                index = self.expect("integer")
                result = ast.D2Eproj(self.integer(index), result)
            elif self.current.kind == "[":
                result = ast.D2Etapp(result, self.static_args())
            else:
                return result

    def integer(self, token):
        try:
            return int(token.value)
        except ValueError:
            self.error("Integer literal exceeds Python's conversion limit", token.start)

    def atom(self):
        if self.current.kind == "(":
            return self.parenthesized(self.expression, ast.D2Etupl)
        token = self.take("integer")
        if token:
            return ast.D2Eint(self.integer(token))
        token = self.take("string")
        if token:
            return ast.D2Estr(token.value)
        if self.take("true"):
            return ast.D2Ebtf(True)
        if self.take("false"):
            return ast.D2Ebtf(False)
        if self.take("@"):
            return ast.D2Ecst(self.expect("identifier").value)
        token = self.take("identifier")
        if token:
            return ast.D2Evar(token.value)
        self.error("Expected expression")

    def complete(self, value):
        self.expect("EOF")
        return value


def s2exp_parse(source: str, *, static_vars: Iterable[str] = (),
                filename: str = "<string>") -> ast.s2exp:
    """Parse a type; static_vars distinguishes variables from constructors."""
    parser = _Parser(source, filename, static_vars)
    return parser.complete(parser.type())


def d2exp_parse(source: str, *, static_vars: Iterable[str] = (),
                filename: str = "<string>") -> ast.d2exp:
    """Parse one complete expression, without evaluating it."""
    parser = _Parser(source, filename, static_vars)
    return parser.complete(parser.expression())


def libfile_parse(source: str, *, filename: str = "<string>") -> ast.d2eclist:
    """Parse zero or more declarations, preserving their source order."""
    parser = _Parser(source, filename, ())
    return parser.complete(parser.declarations())


def program_parse(source: str, *, filename: str = "<string>") -> ast.D2Elets:
    """Parse declarations and a required final expression into D2Elets."""
    parser = _Parser(source, filename, ())
    declarations = parser.declarations()
    expression = parser.expression()
    return parser.complete(ast.D2Elets(declarations, ast.fnoptn_cons(expression)))


# Declaration sequences and library files have the same syntax.
d2eclist_parse = libfile_parse
