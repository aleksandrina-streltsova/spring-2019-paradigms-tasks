#!/usr/bin/env python3
import pytest
from printer import *


def test_conditional():
    conditional1 = Conditional(Number(42), [], [])
    conditional2 = Conditional(Number(42), [Number(21)],
                               [Number(13), Number(14)])
    pretty_printer = PrettyPrinter(ExpressionPrinter())
    result1 = '''\
    if (42) {
    }'''
    result2 = '''\
    if (42) {
        21;
    } else {
        13;
        14;
    }'''
    assert conditional1.accept(pretty_printer) == textwrap.dedent(result1)
    assert conditional2.accept(pretty_printer) == textwrap.dedent(result2)


def test_function_definition():
    function_def1 = FunctionDefinition('foo', Function([], []))
    function_def2 = FunctionDefinition('bar', Function(['a', 'b'],
                                                       [Print(Number(1))]))
    pretty_printer = PrettyPrinter(ExpressionPrinter())
    result1 = '''\
    def foo() {
    }'''
    result2 = '''\
    def bar(a, b) {
        print 1;
    }'''
    assert function_def1.accept(pretty_printer) == textwrap.dedent(result1)
    assert function_def2.accept(pretty_printer) == textwrap.dedent(result2)


def test_print():
    binary_op = BinaryOperation(Number(2), '+', Number(3))
    pretty_printer = PrettyPrinter(ExpressionPrinter())
    assert Print(Number(42)).accept(pretty_printer) == 'print 42;'
    assert Print(Reference('x')).accept(pretty_printer) == 'print x;'
    assert Print(binary_op).accept(pretty_printer) == 'print (2 + 3);'


def test_read():
    assert Read('x').accept(PrettyPrinter(ExpressionPrinter())) == 'read x;'


def test_number():
    assert (Number(10).accept(PrettyPrinter(ExpressionPrinter()))) == '10;'


def test_reference():
    assert (Reference('y').accept(PrettyPrinter(ExpressionPrinter()))) == 'y;'


def test_binary_op():
    pretty_printer = PrettyPrinter(ExpressionPrinter())
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    div = BinaryOperation(mul, '/', Number(5))
    assert mul.accept(pretty_printer) == '(1 * (2 + 3));'
    assert div.accept(pretty_printer) == '((1 * (2 + 3)) / 5);'


def test_unary_op():
    unary_op1 = UnaryOperation('-', Number(42))
    unary_op2 = UnaryOperation('-', unary_op1)
    assert unary_op1.accept(PrettyPrinter(ExpressionPrinter())) == '(-42);'
    assert unary_op2.accept(PrettyPrinter(ExpressionPrinter())) == '(-(-42));'


def test_function_call():
    pretty_printer = PrettyPrinter(ExpressionPrinter())
    function_call = FunctionCall(Reference('foo'),
                                 [Number(1), Number(2), Number(3)])
    assert function_call.accept(pretty_printer) == 'foo(1, 2, 3);'


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
    result = '''\
    def main(arg1) {
        read x;
        print x;
        if ((2 == 3)) {
            if (1) {
            }
        } else {
            exit((-arg1));
        }
    }
    '''
    assert textwrap.dedent(result) == out


if __name__ == "__main__":
    pytest.main()
