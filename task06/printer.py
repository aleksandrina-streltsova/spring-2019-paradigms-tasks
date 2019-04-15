from model import *
import textwrap


class PrettyPrinter(ASTNodeVisitor):
    INDENT = '    '

    def __init__(self):
        self.nesting_level = 0
        self.op_nesting_level = 0

    def visit_conditional(self, conditional):
        result = 'if ('
        self.nesting_level += 1
        result += conditional.condition.accept(self)
        self.nesting_level -= 1
        result += ') {\n'
        result += self.visit_expressions(conditional.if_true)
        result += '}'
        if conditional.if_false:
            result += ' else {\n'
            result += self.visit_expressions(conditional.if_false)
            result += '}'
        return result

    def visit_function_definition(self, function_definition):
        result = 'def '
        result += function_definition.name + '('
        for arg in function_definition.function.args:
            result += arg + ', '
        if function_definition.function.args:
            result = result[:-2]
        result += ') {\n'
        result += self.visit_expressions(function_definition.function.body)
        result += '}'
        return result

    def visit_print(self, print):
        result = 'print '
        self.nesting_level += 1
        result += print.expr.accept(self) + ';'
        self.nesting_level -= 1
        return result

    def visit_read(self, read):
        return 'read ' + read.name + ';'

    def visit_number(self, number):
        result = str(number.value)
        if not self.nesting_level:
            result += ';'
        return result

    def visit_reference(self, reference):
        result = str(reference.name)
        if not self.nesting_level:
            result += ';'
        return result

    def visit_binary_operation(self, binary_op):
        self.op_nesting_level += 1
        self.nesting_level += 1
        result = (binary_op.lhs.accept(self) + ' ' +
                  binary_op.op + ' ' + binary_op.rhs.accept(self))
        self.op_nesting_level -= 1
        self.nesting_level -= 1

        if self.op_nesting_level:
            result = '(' + result + ')'
        if not (self.nesting_level or self.op_nesting_level):
            result += ';'
        return result

    def visit_unary_operation(self, unary_op):
        self.op_nesting_level += 1
        self.nesting_level += 1
        result = unary_op.op + unary_op.expr.accept(self)
        self.op_nesting_level -= 1
        self.nesting_level -= 1

        if self.op_nesting_level:
            result = '(' + result + ')'
        if not (self.nesting_level or self.op_nesting_level):
            result += ';'
        return result

    def visit_function_call(self, function_call):
        self.nesting_level += 1
        result = function_call.fun_expr.accept(self) + '('
        for arg in function_call.args:
            result += arg.accept(self) + ', '
        if function_call.args:
            result = result[:-2]
        result += ')'
        self.nesting_level -= 1

        if not self.nesting_level:
            result += ';'
        return result

    def visit_function(self, function):
        pass

    def visit_expressions(self, expressions):
        result = ''
        for expr in expressions or []:
            result += expr.accept(self) + '\n'
        return textwrap.indent(text=result, prefix=self.INDENT,
                               predicate=lambda line: True)


def pretty_print(program):
    result = program.accept(PrettyPrinter())
    print(result)
