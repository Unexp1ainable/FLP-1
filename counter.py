import numpy as np
from argparse import ArgumentParser


def parse_args():
    parser = ArgumentParser()
    parser.add_argument("file", help="file")
    return parser.parse_args()


def parse_file(filename):
    weights = []
    costs = []
    with open(filename) as file:
        for line in file:
            stripped = line.strip()
            if stripped[0] == "w":
                weights.append(int(stripped[8:]))
            elif stripped[0] == "c":
                costs.append(int(stripped[6:]))
    return weights, costs


if __name__ == "__main__":
    filename = parse_args().file
    weights, costs = parse_file(filename)
    weights = np.array(weights)
    costs = np.array(costs)

    while True:
        inArrStr = input("Input array: ").strip("[]").split(",")
        inArr = [int(num) for num in inArrStr]

        inArrBool = np.array(inArr) == 1
        print(f"Knapsack weight: {np.sum(weights[inArrBool])}")
        print(f"Knapsack cost: {np.sum(costs[inArrBool])}")
