import subprocess

for i in range(2):
    lines = b""
    with open(f"backpacks/backpack{i}.txt", "rb") as file:
        output = subprocess.check_output(["./flp22-fun", "-b"], stdin=file)
        print(output.decode("utf-8"), end="")
