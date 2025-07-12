from python.data import ist
from os import getcwd
from numpy import nan, array
from pandas import DataFrame


aNull = None
anNa = nan
aBool = True

anInt = 19
aDbl = 19.0
aPct = 0.19
aProp = 0
aZero = 0
aOne = 1

aDigit = "19"
anAscii = "a"
nonAscii = "ç"
aDir = getcwd()
aUrl = "https://stackoverflow.com/questions/72923904/regex-for-valid-directory-path"

aList = ["dsds", "lalala"]
aTuple = ("dsds", "lalala")
aVector = array(["dsds", "lalala"], str)
aDf = DataFrame({"dsds": ["lalala"]})

aFam = ist

print(
    "default method (passing type string):",
    ist(aNull, str),
    ist(anNa, str),
    ist(aBool, str),
    ist(anInt, str),
    ist(aDbl, str),
    ist(aPct, str),
    ist(aProp, str),
    ist(aZero, str),
    ist(aOne, str),
    ist(aDigit, str),
    ist(anAscii, str),
    ist(nonAscii, str),
    ist(aDir, str),
    ist(aUrl, str),
    ist(aList, str),
    ist(aTuple, str),
    ist(aVector, str),
    ist(aDf, str),
    ist(aFam, str),
)

print(
    "boolean:",
    ist.bool(aNull),
    ist.bool(anNa),
    ist.bool(aBool),
    ist.bool(anInt),
    ist.bool(aDbl),
    ist.bool(aPct),
    ist.bool(aProp),
    ist.bool(aZero),
    ist.bool(aOne),
    ist.bool(aDigit),
    ist.bool(anAscii),
    ist.bool(nonAscii),
    ist.bool(aDir),
    ist.bool(aUrl),
    ist.bool(aList),
    ist.bool(aTuple),
    ist.bool(aVector),
    ist.bool(aDf),
    ist.bool(aFam),
)

print(
    "callable:",
    ist.callable(aNull),
    ist.callable(anNa),
    ist.callable(aBool),
    ist.callable(anInt),
    ist.callable(aDbl),
    ist.callable(aPct),
    ist.callable(aProp),
    ist.callable(aZero),
    ist.callable(aOne),
    ist.callable(aDigit),
    ist.callable(anAscii),
    ist.callable(nonAscii),
    ist.callable(aDir),
    ist.callable(aUrl),
    ist.callable(aList),
    ist.callable(aTuple),
    ist.callable(aVector),
    ist.callable(aDf),
    ist.callable(aFam),
)

print(
    "character:",
    ist.chr(aNull),
    ist.chr(anNa),
    ist.chr(aBool),
    ist.chr(anInt),
    ist.chr(aDbl),
    ist.chr(aPct),
    ist.chr(aProp),
    ist.chr(aZero),
    ist.chr(aOne),
    ist.chr(aDigit),
    ist.chr(anAscii),
    ist.chr(nonAscii),
    ist.chr(aDir),
    ist.chr(aUrl),
    ist.chr(aList),
    ist.chr(aTuple),
    ist.chr(aVector),
    ist.chr(aDf),
    ist.chr(aFam),
)

print(
    "digit:",
    ist.chr.digit(aNull),
    ist.chr.digit(anNa),
    ist.chr.digit(aBool),
    ist.chr.digit(anInt),
    ist.chr.digit(aDbl),
    ist.chr.digit(aPct),
    ist.chr.digit(aProp),
    ist.chr.digit(aZero),
    ist.chr.digit(aOne),
    ist.chr.digit(aDigit),
    ist.chr.digit(anAscii),
    ist.chr.digit(nonAscii),
    ist.chr.digit(aDir),
    ist.chr.digit(aUrl),
    ist.chr.digit(aList),
    ist.chr.digit(aTuple),
    ist.chr.digit(aVector),
    ist.chr.digit(aDf),
    ist.chr.digit(aFam),
)

print(
    "letter:",
    ist.chr.letter(aNull),
    ist.chr.letter(anNa),
    ist.chr.letter(aBool),
    ist.chr.letter(anInt),
    ist.chr.letter(aDbl),
    ist.chr.letter(aPct),
    ist.chr.letter(aProp),
    ist.chr.letter(aZero),
    ist.chr.letter(aOne),
    ist.chr.letter(aDigit),
    ist.chr.letter(anAscii),
    ist.chr.letter(nonAscii),
    ist.chr.letter(aDir),
    ist.chr.letter(aUrl),
    ist.chr.letter(aList),
    ist.chr.letter(aTuple),
    ist.chr.letter(aVector),
    ist.chr.letter(aDf),
    ist.chr.letter(aFam),
)

print(
    "ascii:",
    ist.chr.letter.ascii(aNull),
    ist.chr.letter.ascii(anNa),
    ist.chr.letter.ascii(aBool),
    ist.chr.letter.ascii(anInt),
    ist.chr.letter.ascii(aDbl),
    ist.chr.letter.ascii(aPct),
    ist.chr.letter.ascii(aProp),
    ist.chr.letter.ascii(aZero),
    ist.chr.letter.ascii(aOne),
    ist.chr.letter.ascii(aDigit),
    ist.chr.letter.ascii(anAscii),
    ist.chr.letter.ascii(nonAscii),
    ist.chr.letter.ascii(aDir),
    ist.chr.letter.ascii(aUrl),
    ist.chr.letter.ascii(aList),
    ist.chr.letter.ascii(aTuple),
    ist.chr.letter.ascii(aVector),
    ist.chr.letter.ascii(aDf),
    ist.chr.letter.ascii(aFam),
)

print(
    "path:",
    ist.chr.path(aNull),
    ist.chr.path(anNa),
    ist.chr.path(aBool),
    ist.chr.path(anInt),
    ist.chr.path(aDbl),
    ist.chr.path(aPct),
    ist.chr.path(aProp),
    ist.chr.path(aZero),
    ist.chr.path(aOne),
    ist.chr.path(aDigit),
    ist.chr.path(anAscii),
    ist.chr.path(nonAscii),
    ist.chr.path(aDir),
    ist.chr.path(aUrl),
    ist.chr.path(aList),
    ist.chr.path(aTuple),
    ist.chr.path(aVector),
    ist.chr.path(aDf),
    ist.chr.path(aFam),
)

print(
    "url:",
    ist.chr.path.url(aNull),
    ist.chr.path.url(anNa),
    ist.chr.path.url(aBool),
    ist.chr.path.url(anInt),
    ist.chr.path.url(aDbl),
    ist.chr.path.url(aPct),
    ist.chr.path.url(aProp),
    ist.chr.path.url(aZero),
    ist.chr.path.url(aOne),
    ist.chr.path.url(aDigit),
    ist.chr.path.url(anAscii),
    ist.chr.path.url(nonAscii),
    ist.chr.path.url(aDir),
    ist.chr.path.url(aUrl),
    ist.chr.path.url(aList),
    ist.chr.path.url(aTuple),
    ist.chr.path.url(aVector),
    ist.chr.path.url(aDf),
    ist.chr.path.url(aFam),
)

print(
    "file:",
    ist.chr.path.file(aNull),
    ist.chr.path.file(anNa),
    ist.chr.path.file(aBool),
    ist.chr.path.file(anInt),
    ist.chr.path.file(aDbl),
    ist.chr.path.file(aPct),
    ist.chr.path.file(aProp),
    ist.chr.path.file(aZero),
    ist.chr.path.file(aOne),
    ist.chr.path.file(aDigit),
    ist.chr.path.file(anAscii),
    ist.chr.path.file(nonAscii),
    ist.chr.path.file(aDir),
    ist.chr.path.file(aUrl),
    ist.chr.path.file(aList),
    ist.chr.path.file(aTuple),
    ist.chr.path.file(aVector),
    ist.chr.path.file(aDf),
    ist.chr.path.file(aFam),
)
