with open("../example.txt") as f:
    lines = f.read().splitlines()


number_words = {
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}


for k, v in number_words.items():
    lines = [s.replace(k, str(v)) for s in lines]

digits = [list(filter(lambda s: s.isdigit(), l)) for l in lines]
numbers = [int(l[0] + l[-1]) for l in digits]
print(numbers)
