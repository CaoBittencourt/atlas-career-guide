from python.base import fam


# default method
def _chr(x):
    return isinstance(x, str)


# subtypes
def _digit(x):
    return all([_chr(x), str(x).isdigit()])


def _letter(x):
    return all([_chr(x), str(x).isalpha()])


# character string family type definitions
chr = fam(
    chr=_chr,
    digit=_digit,
    letter=_letter,
)
