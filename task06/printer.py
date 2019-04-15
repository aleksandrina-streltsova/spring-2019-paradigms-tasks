from model import *
import textwrap


class ExpressionPrinter(ASTNodeVisitor):
    def visit_conditional(self, acceptor):
        pass

    def visit_function_definition(self, acceptor):
        pass

    def visit_print(self, acceptor):
        pass

    def visit_read(self, acceptor):
        pass

    def visit_number(self, number):
        return str(number.value)

    def visit_reference(self, reference):
        return reference.name

    def visit_binary_operation(self, binary_op):
        return ('(' + binary_op.lhs.accept(self) + ' ' +
                binary_op.op + ' ' + binary_op.rhs.accept(self) + ')')

    def visit_unary_operation(self, unary_op):
        return '(' + unary_op.op + unary_op.expr.accept(self) + ')'

    def visit_function_call(self, function_call):
        result = ''
        result += function_call.fun_expr.accept(self) + '('
        for arg in function_call.args:
            result += arg.accept(self) + ', '
        if function_call.args:
            result = result[:-2]
        result += ')'
        return result

    def visit_function(self, acceptor):
        pass


class PrettyPrinter(ASTNodeVisitor):
    INDENT = '    '

    def __init__(self, expression_printer):
        self.expression_printer = expression_printer

    def visit_conditional(self, conditional):
        result = 'if ('
        result += conditional.condition.accept(self.expression_printer)
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
        result += print.expr.accept(self.expression_printer) + ';'
        return result

    def visit_read(self, read):
        return 'read ' + read.name + ';'

    def visit_number(self, number):
        return number.accept(self.expression_printer) + ';'

    def visit_reference(self, reference):
        return reference.accept(self.expression_printer) + ';'

    def visit_binary_operation(self, binary_op):
        return binary_op.accept(self.expression_printer) + ';'

    def visit_unary_operation(self, unary_op):
        return unary_op.accept(self.expression_printer) + ';'

    def visit_function_call(self, function_call):
        return function_call.accept(self.expression_printer) + ';'

    def visit_function(self, function):
        pass

    def visit_expressions(self, expressions):
        result = ''
        for expr in expressions or []:
            result += expr.accept(self) + '\n'
        return textwrap.indent(text=result, prefix=self.INDENT,
                               predicate=lambda line: True)


def pretty_print(program):
    result = program.accept(PrettyPrinter(ExpressionPrinter()))
    print(result)
