import re
import sys
import random
import csv
from tqdm import tqdm

sys.path.append("../code")
from utils import read_lines
from distance import distance_between_formulas
from data_structure import Atom_Features, Symbol_Features


def extract_clause(input_file, output_file):
    lines = read_lines(input_file)
    clauses = [line for line in lines if "cnf" in line and "plain" in line]
    with open(output_file, "w+") as f:
        f.writelines("\n".join(clauses))


def extract_atom(line):
    pattern = re.compile(r"\||\~")
    if "plain" in line:
        raw_split = line.split(", plain, ")
    if "negated_conjecture" in line:
        raw_split = line.split(", negated_conjecture, ")
    # name = raw_split[0].replace("cnf(", "")
    clause = raw_split[1].strip()[1: -3]
    raw_atoms = re.split(pattern, clause)
    atoms = []
    for atom in raw_atoms:
        if atom:
            if "!=" in atom:
                args = atom.split("!=")
                new_atom = "e(" + args[0] + "," + args[1] + ")"
                atoms.append(new_atom)
            elif "=" in atom:
                args = atom.split("=")
                new_atom = "e(" + args[0] + "," + args[1] + ")"
                atoms.append(new_atom)
            else:
                atoms.append(atom)
    return atoms


def extract_symbol(line):
    pattern = re.compile(r"\w+|=|!=")
    if "plain" in line:
        raw_split = line.split(", plain, ")
    if "negated_conjecture" in line:
        raw_split = line.split(", negated_conjecture, ")
    # name = raw_split[0].replace("cnf(", "")
    clause = raw_split[1].strip()[1: -3]
    symbols = re.findall(pattern, clause)
    new_symbols = []
    for sym in symbols:
        if sym == "=" or sym == "!=":
            new_symbols.append("e")
        else:
            new_symbols.append(sym)

    functional_symbols = [
        sym for sym in new_symbols if re.match(r"[A-Z]", sym) is None]
    return functional_symbols


def write_atom_features(clause_file, atom_features_file):
    clauses = read_lines(clause_file)
    with open(atom_features_file, "w+") as f:
        for i, c in enumerate(clauses, 1):
            atoms = extract_atom(c)
            f.write(str(i) + ": " + "  ".join(atoms) + "\n")


def write_symbol_features(clause_file, symbol_features_file):
    clauses = read_lines(clause_file)
    with open(symbol_features_file, "w+") as f:
        for i, c in enumerate(clauses, 1):
            symbols = extract_symbol(c)
            f.write(str(i) + ": " + "  ".join(symbols) + "\n")


# def write_names(clause_file, name_file):
#     clauses = read_lines(clause_file)
#     raw_splits = [line.split(", plain,") for line in clauses]
#     names = [raw_split[0].replace("cnf(", "") for raw_split in raw_splits]
#     with open(name_file, "w+") as f:
#         f.write("\n".join(names))


def problem_weights(symbol_features):
    symbol_set = set()
    for i in range(1, len(symbol_features) + 1):
        symbol_set.update(symbol_features[str(i)])
    f_weights = dict(zip(list(symbol_set), [2.0] * len(symbol_set)))
    v_weight = 1.0
    return f_weights, v_weight


# write_names("./clauseFile.txt", "names")

# write_symbol_features("./clauseFile.txt", "symbol_features")


if __name__ == "__main__":
    write_atom_features("./clauseFile.txt", "./atom_features")
    write_symbol_features("./clauseFile.txt", "./symbol_features")
    symbol_features = Symbol_Features("./symbol_features")
    atom_features = Atom_Features("./atom_features")
    f_weights, v_weight = problem_weights(symbol_features)
    records = []
    for i in range(1, len(symbol_features)):
        for j in range(i + 1, len(symbol_features) + 1):
            d = distance_between_formulas(
                atom_features[str(i)], atom_features[str(j)],
                f_weights, v_weight, "symmetry", True)
            records.append((i, j, d))
    with open("output.csv", "w+") as f:
        csv_writer = csv.writer(f)
        csv_writer.writerow(["clause", "clause", "distance"])
        csv_writer.writerows(records)
