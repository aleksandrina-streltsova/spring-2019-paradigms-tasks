#!/usr/bin/env python3
import pytest
from printer import *


def test_conditional():
    conditional1 = Conditional(Number(42), [], [])
    conditional2 = Conditional(Number(42), [Number(21)],
                               [Number(13), Number(14)])
    result1 = 'if (42) {\n}'
    result2 = 'if (42) {\n    21;\n} else {\n    13;\n    14;\n}'
    assert conditional1.accept(PrettyPrinter()) == result1
    assert conditional2.accept(PrettyPrinter()) == result2


def test_function_definition():
    function_def1 = FunctionDefinition('foo', Function([], []))
    function_def2 = FunctionDefinition('bar', Function(['a', 'b'],
                                                       [Print(Number(1))]))
    result1 = 'def foo() {\n}'
    result2 = 'def bar(a, b) {\n    print 1;\n}'
    assert function_def1.accept(PrettyPrinter()) == result1
    assert function_def2.accept(PrettyPrinter()) == result2


def test_print():
    binary_op = BinaryOperation(Number(2), '+', Number(3))
    assert Print(Number(42)).accept(PrettyPrinter()) == 'print 42;'
    assert Print(Reference('x')).accept(PrettyPrinter()) == 'print x;'
    assert Print(binary_op).accept(PrettyPrinter()) == 'print 2 + 3;'


def test_read():
    assert Read('x').accept(PrettyPrinter()) == 'read x;'


def test_number():
    assert (Number(10).accept(PrettyPrinter())) == '10;'


def test_reference():
    assert (Reference('y').accept(PrettyPrinter())) == 'y;'


def test_binary_op():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    div = BinaryOperation(mul, '/', Number(5))
    assert mul.accept(PrettyPrinter()) == '1 * (2 + 3);'
    assert div.accept(PrettyPrinter()) == '(1 * (2 + 3)) / 5;'


def test_unary_op():
    unary_op1 = UnaryOperation('-', Number(42))
    unary_op2 = UnaryOperation('-', unary_op1)
    assert unary_op1.accept(PrettyPrinter()) == '-42;'
    assert unary_op2.accept(PrettyPrinter()) == '-(-42);'


def test_function_call():
    function_call = FunctionCall(Reference('foo'),
                                 [Number(1), Number(2), Number(3)])
    assert function_call.accept(PrettyPrinter()) == 'foo(1, 2, 3);'


def test_end_to_end(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    result = 'def main(arg1) {\n    read x;\n    print x;\n' \
             '    if (2 == 3) {\n        if (1) {\n        ' \
             '}\n    } else {\n        exit(-arg1);\n    }\n}\n'
    assert result == out


if __name__ == "__main__":
    pytest.main()
