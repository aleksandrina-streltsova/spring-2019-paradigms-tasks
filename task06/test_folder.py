#!/usr/bin/env python3
import pytest
from folder import *
from printer import *


def test_binary_op1(capsys):
    pretty_print(fold_constants(BinaryOperation(Number(1), '+', Number(12))))
    out, err = capsys.readouterr()
    assert out == "13;\n"


def test_binary_op2(capsys):
    pretty_print(fold_constants(BinaryOperation(Reference('x'),
                                                '-', Reference('x'))))
    out, err = capsys.readouterr()
    assert out == "0;\n"


def test_binary_op3(capsys):
    pretty_print(fold_constants(BinaryOperation(Reference('x'),
                                                '*', Number(0))))
    out, err = capsys.readouterr()
    assert out == "0;\n"


def test_binary_op4(capsys):
    pretty_print(fold_constants(BinaryOperation(Number(0),
                                                '*', Reference('x'))))
    out, err = capsys.readouterr()
    assert out == "0;\n"


def test_unary_op(capsys):
    unary_op = UnaryOperation('-', (UnaryOperation('-', Number(13))))
    pretty_print(fold_constants(unary_op))
    out, err = capsys.readouterr()
    assert out == "13;\n"


def test_end_to_end1(capsys):
    pretty_print(fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ))
    out, err = capsys.readouterr()
    assert out == "13;\n"


def test_end_to_end2(capsys):
    pretty_print(fold_constants(FunctionDefinition('main', Function(['x'], [
        Read('y'),
        Print(
            BinaryOperation(
                Number(13),
                '/',
                Number(2)
            )
        ),
        Conditional(
            BinaryOperation(
                Number(2),
                '==',
                BinaryOperation(
                    Number(2),
                    '*',
                    Number(3)
                )
            ),
            [
                Conditional(UnaryOperation('!', Number(5)), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('x'))
                ])
            ],
        ),
    ]))))
    out, err = capsys.readouterr()
    assert out == "def main(x) {\n    read y;\n    print 6;\n    " \
                  "if (0) {\n        if (0) {\n        }\n    } else" \
                  " {\n        exit(-x);\n    }\n}\n"


if __name__ == "__main__":
    pytest.main()
