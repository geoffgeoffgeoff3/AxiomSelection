import csv
import os

from utils import read_lines


def extract_useful_premises_from_E(lines):
    names = []
    for line in lines:
        if "fof" in line and "file" in line:
            names.append(line.split(",")[0].replace("fof(", ""))
    return names


def extract_useful_premises_from_Vampire(lines):
    names = []
    for line in lines:
        if "file" in line:
            names.append(line.split(",")[-1][: -3])
    return names


def compute_selected_problem_from_Vampire(lines):
    names = [line.split(",")[0].replace("tff(", "")
             for line in lines if "tff" in line and "axiom" in line]
    return len(names) + 1


def compute_selected_problem_from_E(lines):
    new_lines = [line for line in lines if "fof" in line and "file" in line]
    return len(new_lines)


def ranking_precision(problem_dir, output_dir, ATP, problem_source):
    filenames = os.listdir(output_dir)
    precision = 0.0
    counter = 0
    for name in filenames:

        output_file = os.path.join(output_dir, name)
        lines = read_lines(output_file)
        if ATP == "E" and "# Proof found!" in lines and \
                "# SZS status Theorem" in lines:
            counter += 1
            useful_names = extract_useful_premises_from_E(lines)

            problem_file = os.path.join(problem_dir, name)
            problem_lines = read_lines(problem_file)

            if problem_source == "Vampire":
                problem_len = compute_selected_problem_from_Vampire(
                    problem_lines)
            if problem_source == "E":
                problem_len = compute_selected_problem_from_E(problem_lines)
            if problem_source == "Q_selection":
                problem_len = len(problem_lines)

            precision += len(useful_names) / problem_len

        if ATP == "Vampire" and "% Refutation found. Thanks to Tanya!" \
                in lines:
            counter += 1
            useful_names = extract_useful_premises_from_Vampire(lines)

            problem_file = os.path.join(problem_dir, name)
            problem_lines = read_lines(problem_file)

            if problem_source == "Vampire":
                problem_len = compute_selected_problem_from_Vampire(
                    problem_lines)
            if problem_source == "E":
                problem_len = compute_selected_problem_from_E(problem_lines)
            if problem_source == "Q_selection":
                problem_len = len(problem_lines)

            precision += len(useful_names) / problem_len

    precision = precision / counter
    return precision, counter


def ranking_density(proofs, rankings):

    sum_density = 0.0

    for thm_name in rankings:
        useful_prem_list = proofs[thm_name]
        ranked_premises = rankings[thm_name]

        temp = []
        for useful_prems in useful_prem_list:
            try:
                max_index = max([ranked_premises.index(prem)
                                 for prem in useful_prems])
            except:
                max_index = - 1

            temp.append((len(useful_prems) + 1) / (max_index + 2))

        sum_density += max(temp)

    density = sum_density / len(rankings)

    return density
