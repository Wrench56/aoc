def _apply_sol(inp: list[str], func: callable) -> str:
    invl = []
    for rng in inp[0].split(','):
        b, e = rng.split('-')
        for num in range(int(b), int(e) + 1):
            if func(num):
                invl.append(num)
    return str(sum(invl))

def part1(inp: list[str]) -> str:
    def is_invalid(num: int) -> bool:
        s = str(num)
        if (len(s) % 2 != 0):
            return False
        mid = int(len(s) / 2)
        if s[:mid] == s[mid:]:
            return True
        return False

    return _apply_sol(inp, is_invalid)

def part2(inp: list[str]) -> str:
    def is_invalid(num: int) -> bool:
        s = str(num)
        return s in (s+s)[1:-1]

    return _apply_sol(inp, is_invalid)
