from lark import Lark, Transformer


object_parser = Lark(r"""
    object: functor "(" arguments ")" | functor | variable | bool

    arguments: object ("," object)*

    functor: FUNCTOR
    variable: VARIABLE
    bool: BOOL

    FUNCTOR: (LCASE_LETTER | DIGIT) (LCASE_LETTER | DIGIT | UCASE_LETTER | "_")*
    VARIABLE: UCASE_LETTER (LCASE_LETTER | DIGIT | UCASE_LETTER | "_")*
    BOOL: "$true"
    %import common.DIGIT
    %import common.LCASE_LETTER
    %import common.UCASE_LETTER
    %import common.WS
    %ignore WS 
    """, start="object")


class My_Transformer(Transformer):
    object = lambda self, a: a 
    functor = lambda self, a: a[0][:] 
    variable = lambda self, a: a[0][:]
    bool = lambda self, a: a[0][:]
    arguments = lambda self, a: a


def parser(string):
    tree_list = My_Transformer().transform(object_parser.parse(string))
    return tree_list


class Term():

    def __init__(self, term, root_index="0"):
        self.id2subterm = dict()
        self.id2symbol = dict()
        self.tree = {root_index: self.build_term_dict(term, root_index)}

    def build_term_dict(self, object, root_index="0"):
        self.id2subterm[root_index] = object
        self.id2symbol[root_index] = object[0]
        if len(object) == 2 and isinstance(object[0], str) and \
                isinstance(object[1], list):
            tree = dict()
            tree[object[0]] = dict()
            for i, arg in enumerate(object[1]):
                index = root_index + str(i)
                tree[object[0]][index] = self.build_term_dict(arg, index)
        else:
            tree = object[0]
        return tree
