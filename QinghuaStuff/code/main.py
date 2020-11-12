from data_structure import Atom_Features, Symbol_Features, \
    Chronology, Problem_Order
from selection import problem_ranking, problem_weights, Qinf_selection

if __name__ == "__main__":
    atom_features = Atom_Features("../data/atom_features")
    symbol_features = Symbol_Features("../data/symbol_features")
    chronology = Chronology("../data/chronology")
    problem_order = Problem_Order("../data/ProblemsInMMLOrder")

    for thm in [problem_order[0]]:
        available_prems = chronology.available_premises(thm)
        f_weights, v_weight = problem_weights(
            thm, available_prems, symbol_features)
        scores = problem_ranking(thm, available_prems,
                                 atom_features, f_weights, v_weight)
        selected_prems = Qinf_selection(scores)
        # print(scores)
        # print(selected_prems)
