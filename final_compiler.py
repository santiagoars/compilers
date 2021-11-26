import ply.lex as lex
import ply.yacc as yacc

literals = ['(', ')', '{', '}', '=', '+', '-', '*', '/', '^', '<', '>', ';']
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
tokens = ['FLOATV',
          'INTV',
          'EQC',
          'NOTEQC',
          'BIGGEREQ',
          'SMALLEREQ',
          'STRINGV',
          'ID'] + list(reserved.values())

t_EQC = r'=='
t_NOTEQC = r'!='
t_BIGGEREQ = r'>='
t_SMALLEREQ = r'<='


def t_FLOATV(x):
    r'\d+\.\d+'
    x.value = float(x.value)
    return x


def t_INTV(x):
    r'\d+'
    x.value = int(x.value)
    return x


def t_STRING(x):
    r'".*"'
    x.value = x.value.replace("\"", "")
    x.type = reserved.get(x.value, 'STRINGV')
    return x


def t_ID(x):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    x.type = reserved.get(x.value, 'ID')
    return x


def t_newline(x):
    r'\n+'
    x.lexer.lineno += len(x.value)


t_ignore = ' \t'


def t_error(x):
    print("Error, caracter ilegal: '%s'" % x.value[0])
    x.lexer.skip(1)


lex.lex()
precedence = (
    ('right', '='),
    ('left', 'EQC', 'NOTEQC'),
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('left', '^'),
    ('left', 'AND', 'OR'),
    ('nonassoc', '<', '>', 'BIGGEREQ', 'SMALLEREQ'),
    ('right', 'UMINUS')
)
names = {}
prog = {}


def p_start(y):
    '''prog : statement'''
    global prog
    prog = y[1]


def p_statement(y):
    '''statement : conditional statement
                 | while statement
                 | for statement
                 | declare ';' statement
                 | print ';' statement
                 | none'''
    if len(y) > 2:
        if y[2] == ';':
            y[2] = y[3]
        y[0] = (y[1],) + y[2]
    else:
        y[0] = ()


def p_none(y):
    'none :'
    pass


def p_conditional(y):
    '''conditional : if elif else'''
    y[0] = ('conditional', y[1], y[2], y[3])


def p_if(y):
    '''if : IF '(' expression ')' '{' statement '}' '''
    y[0] = ('if', y[3], y[6])


def p_elif(y):
    '''elif : ELIF '(' expression ')' '{' statement '}' elif
                 | none'''
    if len(y) > 2:
        y[0] = (('elif', y[3], y[6]),) + y[8]
    else:
        y[0] = ()


def p_else(y):
    '''else : ELSE '{' statement '}'
            | none'''
    if len(y) > 2:
        y[0] = ('else', y[3])


def p_while(y):
    '''while : WHILE '(' expression ')' '{' statement '}'
             | DO '{' statement '}' WHILE '(' expression ')' ';' '''
    if y[1] == "while":
        y[0] = ('while', y[3], y[6])
    else:
        y[0] = ('do-while', y[7], y[3])


def p_for(y):
    '''for : FOR '(' declarationAssign ';' expression ';' declareAssign ')' '{' statement '}' '''
    y[0] = ('for', y[3], y[5], y[7], y[10])


def p_type(y):
    '''type : INT
            | FLOAT
            | STRING
            | BOOLEAN'''
    y[0] = y[1]


def p_declare(y):
    '''declare : declaration
               | declarationAssign
               | declareAssign'''
    y[0] = y[1]


def p_declaration(y):
    '''declaration : type ID'''
    y[0] = ('declare', y[1], y[2])


def p_declarationAssign(y):
    '''declarationAssign : type ID '=' expression'''
    y[0] = ('declareAssign', y[1], y[2], y[4])


def p_declareAssign(y):
    '''declareAssign : ID '=' expression'''
    y[0] = ('assign', y[1], y[3])
    y[0] = ('assign', y[1], y[3])


def p_print(y):
    'print : PRINT expression'
    y[0] = ('print', y[2])


def p_expression_operation(y):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression
                  | expression '^' expression
                  | expression '>' expression
                  | expression '<' expression
                  | expression AND expression
                  | expression OR expression
                  | expression EQC expression
                  | expression NOTEQC expression
                  | expression BIGGEREQ expression
                  | expression SMALLEREQ expression'''
    y[0] = ('operation', y[1], y[2], y[3])


def p_expression_uminus(y):
    '''expression : '-' expression %prec UMINUS'''
    y[0] = -y[2]


def p_expression_group(y):
    '''expression : '(' expression ')' '''
    y[0] = y[2]


def p_expression_number(y):
    '''expression : INTV
                  | FLOATV
                  | STRINGV
                  | boolval'''
    y[0] = y[1]


def p_boolVal(y):
    '''boolval : TRUE
               | FALSE'''
    if y[1] == "true":
        y[0] = True
    elif y[1] == "false":
        y[0] = False


def p_expression_ID(y):
    "expression : ID"
    y[0] = y[1]


def p_error(y):
    if y:
        print("Error de sintaxis: '%s'" % y.value)
    else:
        print("Error de sintaxis (EOF)")

yacc.yacc()
file = open("test.txt", "r")
s = file.read()
yacc.parse(s)    

code = []

def d_assign(val):
    res = "" + val[2] + " "
    if val[1] == 'int' or val[1] == 'float':
        if type(val[3]) is not tuple:
            res = res + str(float(val[3]))
        else:
            res = res + oper(val[3])
    else:
        res = res + str(val[3])

    code.append(res)


def declare(val):
    res = "" + val[1] + " " + val[2]
    code.append(res)


def oper(val):
    operators = ['>', '>=', '<', '<=', 'and', 'or', '==']
    res = ""
    if val[2] in operators:
        if type(val[1]) is not tuple:
            res = res + str(val[1])
        else:
            res = res + str(oper(val[1]))

        res = res + ' ' + val[2] + ' '

        if type(val[3]) is not tuple:
            res = res + str(val[3])
        else:
            res = res + str(oper(val[3]))

        code.append(res)

    else:
        num = 0
        if type(val[1]) is not tuple:
            if type(val[1]) is int or float:
                num = val[1]

            res = res + str(val[1])
        else:
            num = oper(val[1])
            res = res + str(num)

        if val[2] == '+':
            res = res + ' + '
        elif val[2] == '-':
            res = res + ' - '
        elif val[2] == '*':
            res = res + ' * '
        elif val[2] == '/':
            res = res + ' / '
        elif val[2] == '^':
            res = res + ' ^ '

        if type(val[3]) is not tuple:
            if val[2] == '+' and type(val[3]) is not str:
                if type(num) is not str:
                    num = num + val[3]
                else:
                    num = val[3]
            elif val[2] == '-' and type(val[3]) is not str:
                if type(num) is not str:
                    num = num - val[3]
                else:
                    num = val[3]
            elif val[2] == '*' and type(val[3]) is not str:
                if type(num) is not str:
                    num = num * val[3]
                else:
                    num = val[3]
            elif val[2] == '/' and type(val[3]) is not str:
                if type(num) is not str:
                    num = num / val[3]
                else:
                    num = val[3]
            elif val[2] == '^' and type(val[3]) is not str:
                if type(num) is not str:
                    num = num ** val[3]
                else:
                    num = val[3]

            res = res + str(val[3])

        elif type(val[3]) is str:
            res = res + val[3]
            code.append(res)
            return res

        else:
            operationRes = 0
            if val[2] == '+':
                op = oper(val[3])
                num = num + op
                operationRes = op
            elif val[2] == '-':
                op = oper(val[3])
                num = num - op
                operationRes = op
            elif val[2] == '*':
                op = oper(val[3])
                num = num * op
                operationRes = op
            elif val[2] == '/':
                op = oper(val[3])
                num = num / op
                operationRes = op
            elif val[2] == '^':
                op = oper(val[3])
                num = num ** op
                operationRes = op

            res = res + str(operationRes)

        code.append(res)
        return num


def assign(val):
    res = "" + val[1] + " "
    if type(val[2]) is not tuple:
        res = res + str(val[2])
    else:
        res = res + str(oper(val[2]))
    code.append(res)


def print_operation(val):
    res = val[0] + ' ' + val[1]
    code.append(res)

def if_cond(val):
    code.append('if')
    oper(val[1][1])
    for statement in val[1][2]:
        if statement[0] == 'declareAssign':
            d_assign(statement)
        elif statement[0] == 'declare':
            declare(statement)
        elif statement[0] == 'assign':
            assign(statement)
        elif statement[0] == 'print':
            print_operation(statement)
        elif statement[0] == 'if':
            if_cond(statement[1])
        elif statement[0] == 'while':
            while_loop(statement)
    code.append('end if')
    index = 2
    while index < len(val):
        if len(val[index]) > 0:
            if val[index][0][0] == 'elif':
                code.append('elif')
                oper(val[index][0][1])
                for statement in val[index][0][2]:
                    if statement[0] == 'declareAssign':
                        d_assign(statement)
                    elif statement[0] == 'declare':
                        declare(statement)
                    elif statement[0] == 'assign':
                        assign(statement)
                    elif statement[0] == 'print':
                        print_operation(statement)
                    elif statement[0] == 'if':
                        if_cond(statement[1])
                    elif statement[0] == 'while':
                        while_loop(statement)
                code.append('end elif')
            else:
                code.append('else')
                for statement in val[index][1]:
                    if statement[0] == 'declareAssign':
                        d_assign(statement)
                    elif statement[0] == 'declare':
                        declare(statement)
                    elif statement[0] == 'assign':
                        assign(statement)
                    elif statement[0] == 'print':
                        print_operation(statement)
                    elif statement[0] == 'if':
                        if_cond(statement[1])
                    elif statement[0] == 'while':
                        while_loop(statement)
                code.append('end else')

        index = index + 1


def while_loop(val):
    code.append('while')
    oper(val[1])
    for statement in val[2]:
        if statement[0] == 'declareAssign':
            d_assign(statement)
        elif statement[0] == 'declare':
            declare(statement)
        elif statement[0] == 'assign':
            assign(statement)
        elif statement[0] == 'print':
            print_operation(statement)
        elif statement[0] == 'conditional':
            if_cond(statement)
        elif statement[0] == 'while':
            while_loop(statement)
    code.append('end while')

for val in prog:
    if val[0] == 'declareAssign':
        d_assign(val)
    elif val[0] == 'declare':
        declare(val)
    elif val[0] == 'assign':
        assign(val)
    elif val[0] == 'print':
        print_operation(val)
    elif val[0] == 'conditional':
        if_cond(val)
    elif val[0] == 'while':
        while_loop(val)

file = open('result.txt', 'a')
for val in code:
    file.write(val + '\n')
file.close()
