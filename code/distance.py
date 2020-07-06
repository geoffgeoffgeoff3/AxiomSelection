import re
from math import log
import numpy as np
from structure import Term
from lgg import construct_lgg, rename, parser


def split_substitutions(substitutions):
    var_part = dict()
    fun_part = dict()
    for key in substitutions:
        value = substitutions[key]
        if len(value) == 1 and re.match(r"[A-Z]", value[0]) is not None:
            var_part[key] = value
        else:
            fun_part[key] = value
    return fun_part, var_part


def weight_of_functional_term(term, f_weights):
    """
    Args:
        term: the tree_list
        f_weights: the functional weight dict
    Return:
        the weight of the functional term
    """
    id2symbol = Term(term).id2symbol
    weights = 0.0
    f_symbols = [symbol for symbol in list(id2symbol.values())
                 if re.match(r"[A-Z]", symbol) is None]
    for symbol in f_symbols:
        weights += f_weights[symbol]
    return weights


def occurence_of_variable(id2symbol, variable, nest=False):
    variables = [symbol for symbol in list(id2symbol.values())
                 if re.match(r"[A-Z]", symbol) is not None]
    if not nest:
        occurence = variables.count(variable)
    else:
        occurence = 0
        indexs = [index for index in id2symbol if id2symbol[index] == variable]
        for index in indexs:
            occurence += len(index)
    return occurence


def functional_distance(lgg, f_subs, f_weights):
    lgg_id2symbol = Term(lgg).id2symbol
    f_distance = 0.0
    if f_subs:
        for var in f_subs:
            f_term = f_subs[var]
            term_weight = weight_of_functional_term(f_term, f_weights)
            var_occs = occurence_of_variable(lgg_id2symbol, var, nest=False)
            f_distance += term_weight * var_occs
    return f_distance


def variable_distance(lgg, term, v_subs, v_weight):
    v_distance = 0.0
    if v_subs:
        lgg_id2symbol = Term(lgg).id2symbol
        term_id2symbol = Term(term).id2symbol
        for var in v_subs:
            mapped_var = v_subs[var][0]
            var_occ = occurence_of_variable(lgg_id2symbol, var, nest=True)
            mapped_var_occ = occurence_of_variable(term_id2symbol, mapped_var,
                                                   nest=True)
            v_distance += log(v_weight * (mapped_var_occ - var_occ) + 1)
    return v_distance


def distance_bwtween_terms(term1, term2, f_weights, v_weight):
    lgg, sub1, sub2 = construct_lgg(term1, term2)
    term1 = parser(rename(term1, "V"))
    term2 = parser(rename(term2, "VV"))
    f_sub1, v_sub1 = split_substitutions(sub1)
    f_sub2, v_sub2 = split_substitutions(sub2)

    f_distance1 = functional_distance(lgg, f_sub1, f_weights)
    f_distance2 = functional_distance(lgg, f_sub2, f_weights)

    v_distance1 = variable_distance(lgg, term1, v_sub1, v_weight)
    v_distance2 = variable_distance(lgg, term2, v_sub2, v_weight)

    distance1 = np.array([f_distance1, v_distance1])
    distance2 = np.array([f_distance2, v_distance2])
    distance = distance1 + distance2
    return distance


def distance_between_atoms(atom1, atom2, f_weights, v_weight):
    if parser(atom1)[0] != parser(atom2)[0]:
        distance = np.array([float("inf"), float("inf")])
    else:
        distance = distance_bwtween_terms(atom1, atom2, f_weights, v_weight)
    return distance


def compute_noninfinity(sorted_distances):
    try:
        index = sorted_distances.index(float('inf'))
        n = index
    except:
        n = len(sorted_distances)
    return n


def distance_between_formulas(formula1, formula2, f_weights, v_weight):
    atom_distances = [distance_between_atoms(atom1, atom2, f_weights, v_weight)
                      for atom1 in formula1 for atom2 in formula2]
    combined_distances = [np.sqrt(np.sum(np.power(score, 2)))
                          for score in atom_distances]
    sorted_distances = sorted(combined_distances)
    non_inf_num = compute_noninfinity(sorted_distances)
    if non_inf_num == 0:
        distance = np.float("inf")
    else:
        distance = np.sum(np.array(sorted_distances[: non_inf_num])) * \
            len(sorted_distances) / pow(non_inf_num, 2)
    return distance
