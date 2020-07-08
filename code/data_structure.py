from utils import read_lines


class Atom_Features:

    def __init__(self, file_path):
        self.atom_features = self.build_features(file_path)

    def __len__(self):
        return len(self.atom_features)

    def __iter__(self):
        return self.atom_features.__iter__()

    def __getitem__(self, name):
        return self.atom_features[name]

    def __contains__(self, thm):
        return thm in self.atom_features

    def build_features(self, file_path):
        lines = read_lines(file_path)
        row_splits = [line.split(": ") for line in lines]
        names = [row_split[0] for row_split in row_splits]
        atom_lists = [row_split[1].strip().split("  ")
                      for row_split in row_splits]
        name2atom = dict(zip(names, atom_lists))
        return name2atom

class Symbol_Features:

    def __init__(self, file_path):
        self.symbol_features = self.build_symbol_features(file_path)

    def __len__(self):
        return len(self.symbol_features)

    def __iter__(self):
        return self.symbol_features.__iter__()

    def __getitem__(self, thm):
        return self.symbol_features[thm]

    def __contains__(self, thm):
        return thm in self.symbol_features

    def build_symbol_features(self, file_path):
        lines = read_lines(file_path)
        row_splits = [line.split(": ") for line in lines]
        names = [row_split[0] for row_split in row_splits]
        symbol_lists = [row_split[1].strip().split("  ")
                        for row_split in row_splits]
        name2symbol = dict(zip(names, symbol_lists))
        return name2symbol


class Statements:

    def __init__(self, file_path):
        self.statements = self.build_statements(file_path)

    def __len__(self):
        return len(self.statements)

    def __iter__(self):
        return self.statements.__iter__()

    def __getitem__(self, thm):
        return self.statements[thm]

    def __contains__(self, thm):
        return thm in self.statements

    def build_statements(self, file_path):
        lines = read_lines(file_path)
        names = [line.split(", ")[0].replace("fof(", "") for line in lines]
        lines = [line.strip().replace(" ", "") for line in lines]
        name2statement = dict(zip(names, lines))
        return name2statement

class Chronology:

    def __init__(self, file_path):
        self.chronology = self.build_chronology(file_path)

    def __len__(self):
        return len(self.chronology)

    def __getitem__(self, index):
        return self.chronology[index]

    def __contains__(self, thm):
        return thm in self.chronology

    def index(self, thm):
        if thm in set(self.chronology):
            return self.chronology.index(thm)

    def available_premises(self, thm):
        if thm in self.chronology:
            return self.chronology[:self.index(thm)]

    def build_chronology(self, file_path):
        return read_lines(file_path)


class Problem_Order():
    def __init__(self, file_path):
        self.problem_order = self.build_problem_order(file_path)

    def __len__(self):
        return len(self.theorem_order)

    def __contains__(self, thm):
        return thm in self.theorem_order

    def __getitem__(self, index):
        return self.problem_order[index]

    def build_problem_order(self, file_path):
        return read_lines(file_path)
