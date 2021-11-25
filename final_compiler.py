import ply.lex as lex
import ply.yacc as yacc

literals = ['(', ')', '{', '}','=', '+', '-', '*', '/', '^', '<', '>', ';']
reserved = {
    'float': 'FLOAT',
    'string': 'STRING',
    'boolean': 'BOOLEAN',
    'true': 'TRUE',
    'false': 'FALSE',
    'and': 'AND',
    'or': 'OR',
    'int': 'INT',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',
    'do': 'DO',
    'while': 'WHILE',
    'for': 'FOR',
    'print': 'PRINT'
}
t_EQC = r'=='
t_NOTEQC = r'!='
t_BIGGEREQ = r'>='
t_SMALLEREQ = r'<='

def float_value(x):
    r'\d+\.\d+'
    x.value = float(x.value)
    return x


def int_value(x):
    r'\d+'
    x.value = int(x.value)
    return x


def string_value(x):
    r'".*"'
    x.value = x.value.replace("\"", "")
    x.type = reserved.get(x.value, 'STRINGV')
    return x


def id(x):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    x.type = reserved.get(x.value, 'ID')
    return x


def new_line(x):
    r'\n+'
    x.lexer.lineno += len(x.value)


t_ignore = ' \t'


def t_error(t):
    print("Error, caracter ilegal: '%s'" % t.value[0])
    t.lexer.skip(1)

