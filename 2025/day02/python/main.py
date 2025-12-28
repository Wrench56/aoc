import sys
import time

import solution

def exec_time(func: callable, partnum: int, inp: str) -> str:
    start = time.perf_counter()
    solstr = func(inp)
    stop = time.perf_counter()
    print(f'Part{partnum} executed in {(stop - start):.4f}s')
    return solstr

def main() -> None:
    fname = '../input.txt'
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    with open(fname, 'r', encoding='utf-8') as ifile:
        inp = ifile.readlines()
    solstr = exec_time(solution.part1, 1, inp)
    print(f'Output:\n{solstr}')
    if solstr != '':
        print(f'Output:\n{exec_time(solution.part2, 2, inp)}')


if __name__ == '__main__':
    main()
