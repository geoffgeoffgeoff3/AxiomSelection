import re
from structure import Term, parser


def rename(obj, ex_str):
    pattern = re.compile(r"[A-Z]\w*")
    try:
        variables = set(re.findall(pattern, obj))
        for var in variables:
            obj = re.sub(var, ex_str + var, obj)
    except:
        obj = obj
    return obj


def diff_subterm(obj1, obj2):
    for id in obj1.id2symbol:
        if id in obj2.id2symbol:
            if obj1.id2symbol[id] != obj2.id2symbol[id]:
                return obj1.id2subterm[id], obj2.id2subterm[id]


def same_id(obj1, obj2, subterm1, subterm2):
    ids_1 = [id for id in obj1.id2subterm
             if obj1.id2subterm[id] == subterm1]
    ids_2 = [id for id in obj2.id2subterm
             if obj2.id2subterm[id] == subterm2]
    return list(set(ids_1) & set(ids_2))


def change_value(term_dict, id, new_value):
    if isinstance(term_dict, dict):
        for key in term_dict:
            if key == id:
                term_dict[key] = new_value
            else:
                change_value(term_dict[key], id, new_value)
    return term_dict


def unification(obj1, obj2, new_var):
    subterm1, subterm2 = diff_subterm(obj1, obj2)
    ids = same_id(obj1, obj2, subterm1, subterm2)
    # new_var = same_name_checking(t1, t2)
    substitution1 = {new_var: subterm1}
    substitution2 = {new_var: subterm2}
    for key in ids:
        tree1 = change_value(obj1.tree, key, new_var)
        tree2 = change_value(obj2.tree, key, new_var)
    return tree1, tree2, substitution1, substitution2


def reconstruct_obj(tree):
    new_obj = []
    for key in tree:
        if not key.isdigit():
            new_obj.append(key)
        if isinstance(tree[key], dict):
            new_obj.append(reconstruct_obj(tree[key]))
        else:
            new_obj.append([tree[key]])
    return new_obj


def construct_lgg(obj1, obj2):
    obj1 = parser(rename(obj1, "V"))
    obj2 = parser(rename(obj2, "VV"))
    
    substitutions1 = dict()
    substitutions2 = dict()
    var_index = 0
    while obj1 != obj2:
        t1 = Term(obj1)
        t2 = Term(obj2)
        tree1, tree2, substitution1, substitution2 = \
            unification(t1, t2, 'VAR' + str(var_index))
        substitutions1.update(substitution1)
        substitutions2.update(substitution2)
        obj1 = reconstruct_obj(tree1)
        obj2 = reconstruct_obj(tree2)
        obj1 = obj1[0]
        obj2 = obj2[0]
        var_index += 1
    assert obj1 == obj2
    lgg = obj1
    return lgg, substitutions1, substitutions2
