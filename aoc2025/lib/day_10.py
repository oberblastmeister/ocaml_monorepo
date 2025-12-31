# /// script
# dependencies = [
#   "z3-solver>=4.15.4.0",
# ]
# ///

import z3


def parse_line(line):
    indicators_part, rest = line.split("]", 1)

    indicators = indicators_part.lstrip("[").strip()

    buttons_part, requirements_part = rest.split("{", 1)
    buttons_part = buttons_part.strip()

    requirements_part = requirements_part.rstrip("}").strip()

    indicators_value = 0
    for i, c in enumerate(indicators):
        if c == "#":
            indicators_value |= 1 << i
        elif c == ".":
            pass  # Remains 0
        else:
            raise ValueError(f"Unexpected character in indicators: {c}")

    buttons = []
    for button in buttons_part.split():
        button = button.strip("()")
        coords = [int(x) for x in button.split(",")]
        buttons.append(coords)

    requirements = [int(x) for x in requirements_part.split(",")]

    return indicators_value, buttons, requirements


def part2(input: str):
    lines = input.splitlines()
    total = 0
    for line in lines:
        _, buttons, v = parse_line(line)
        A = []
        for button in buttons:
            w = [0] * len(v)
            for i in button:
                w[i] += 1
            A.append(w)

        s = z3.Optimize()
        c = [z3.Int(f"c{i}") for i in range(len(A))]
        for ci in c:
            s.add(ci >= 0)
        for j in range(len(A[0])):
            s.add(sum(A[i][j] * c[i] for i in range(len(A))) == v[j])
        s.minimize(sum(c))
        assert s.check() == z3.sat
        model = s.model()
        res = sum(model[ci].as_long() for ci in c)
        total += res
    print(total)


with open("input/10") as file:
    input = file.read()
    part2(input)
