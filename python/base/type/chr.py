from python.base import fam
from re import fullmatch


# default method
def _chr(x):
    return isinstance(x, str)


# subtypes
def _digit(x):
    return all([_chr(x), str(x).isdigit()])


def _letter(x):
    return all([_chr(x), str(x).isalpha()])


def _ascii(x):
    return all([_letter(x), str(x).isascii()])


def _url(x):
    return (
        False
        if not _chr(x)
        else (
            True
            if fullmatch(
                r"(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})",
                x,
            )
            else False
        )
    )


def _path(x):
    return (
        False
        if not _chr(x)
        else (
            True
            if fullmatch(
                r"^(.+)\/([^\/]+)$",
                x,
            )
            else False
        )
    )


def _file(x):
    # task: define a file
    return all([_path(x)])


def _dir(x):
    return all([_path(x), not _file(x)])


# character string family type definitions
chr = fam(
    chr=_chr,
    path=fam(
        path=_path,
        dir=_dir,
        file=_file,
        url=_url,
    ),
    digit=_digit,
    letter=fam(
        letter=_letter,
        ascii=_ascii,
    ),
)
