import subprocess
from counter import *

for i in range(50):
    lines = b""
    ref_result = ""
    test_result = ""
    fname = f"backpacks/backpack{i}.txt"
    print(f"----- {fname} -----")

    with open(fname, "rb") as file:
        ref = subprocess.run(["./flp22-fun", "-b"], stdin=file, stdout=subprocess.PIPE)
        ref_result = ref.stdout.decode("utf-8").strip()

    with open(f"backpacks/backpack{i}.txt", "rb") as file:
        test = subprocess.run(["./flp22-fun", "-o"], stdin=file, stdout=subprocess.PIPE)
        test_result = test.stdout.decode("utf-8").strip()

    weights, costs = parse_file(fname)
    weights = np.array(weights)
    costs = np.array(costs)

    print("Reference")
    print(ref_result.strip())
    if ref_result != "False":
        inArrStr = ref_result.strip("[]").split(",")
        inArr = [int(num) for num in inArrStr]

        w, c = sum_stuff(inArr, weights, costs)
        print(f"Knapsack weight: {w}")
        print(f"Knapsack cost: {c}")

    print("Test")
    print(test_result.strip())
    if test_result != "False":
        inArrStr = test_result.strip("[]").split(",")
        inArr = [int(num) for num in inArrStr]

        w, c = sum_stuff(inArr, weights, costs)
        print(f"Knapsack weight: {w}")
        print(f"Knapsack cost: {c}")
