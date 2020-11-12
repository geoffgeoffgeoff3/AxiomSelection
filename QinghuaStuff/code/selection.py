from distance import distance_between_formulas, compute_noninfinity


def problem_weights(theorem, available_prems, symbol_features, type="default"):
    if type == "default":
        symbol_set = set()
        for name in available_prems + [theorem]:
            symbol_set.update(symbol_features[name])

        f_weights = dict(zip(list(symbol_set), [2.0] * len(symbol_set)))
        v_weight = 1.0
        return f_weights, v_weight


def problem_ranking(theorem, available_prems, atom_features,
                    f_weights, v_weight):
    distances = []
    for prem in available_prems:
        d = distance_between_formulas(
            atom_features[theorem], atom_features[prem], f_weights, v_weight)
        distances.append(d)

    prem2score = dict(zip(available_prems, distances))
    sorted_prem2score = sorted(prem2score.items(), key=lambda x: x[1])
    return sorted_prem2score


def Qinf_selection(sorted_prem2score):
    scores = [pair[1] for pair in sorted_prem2score]
    prems = [pair[0] for pair in sorted_prem2score]
    non_inf_num = compute_noninfinity(scores)
    selected_prems = prems[: non_inf_num]
    return selected_prems
