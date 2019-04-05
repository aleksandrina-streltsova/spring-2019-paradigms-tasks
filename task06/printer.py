from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.number_of_indents = 0
        self.nesting_level = 0
        self.op_nesting_level = 0

    def visit_conditional(self, conditional):
        result = self.number_of_indents * '    ' + 'if ('
        self.nesting_level += 1
        result += conditional.condition.accept(self)
        self.nesting_level -= 1
        result += ') {\n'

        self.number_of_indents += 1
        for expr in conditional.if_true:
            result += expr.accept(self) + '\n'
        self.number_of_indents -= 1
        result += self.number_of_indents * '    ' + '}'

        if conditional.if_false:
            result += ' else {\n'
            self.number_of_indents += 1
            for expr in conditional.if_false:
                result += expr.accept(self) + '\n'
            self.number_of_indents -= 1
            result += self.number_of_indents * '    ' + '}'

        return result

    def visit_function_definition(self, function_definition):
        result = self.number_of_indents * '    ' + 'def '
        result += function_definition.name + '('
        for arg in function_definition.function.args:
            result += arg + ', '
        if function_definition.function.args:
            result = result[:-2]
        result += ') {\n'

        self.number_of_indents += 1
        for statement in function_definition.function.body:
            result += statement.accept(self) + '\n'
        self.number_of_indents -= 1

        result += '}'
        return result

    def visit_print(self, print):
        result = self.number_of_indents * '    ' + 'print '
        self.nesting_level += 1
        result += print.expr.accept(self) + ';'
        self.nesting_level -= 1
        return result

    def visit_read(self, read):
        return self.number_of_indents * '    ' + 'read ' + read.name + ';'

    def visit_number(self, number):
        result = str(number.value)
        if not self.nesting_level:
            result = self.number_of_indents * '    ' + result + ';'
        return result

    def visit_reference(self, reference):
        result = str(reference.name)
        if not self.nesting_level:
            result = self.number_of_indents * '    ' + result + ';'
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
            result = self.number_of_indents * '    ' + result + ';'
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
            result = self.number_of_indents * '    ' + result + ';'
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
            result = self.number_of_indents * '    ' + result + ';'
        return result

    def visit_function(self, function):
        pass


def pretty_print(program):
    result = program.accept(PrettyPrinter())
    print(result)
