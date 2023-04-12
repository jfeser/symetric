#!/usr/bin/env python

__doc__ = """
Usage: run_regex_gpt.py
"""

from docopt import docopt
import openai
import os
import subprocess


numeric = True
prompt_file = "csg_numeric_prompt.txt" if numeric else "csg_prompt.txt"


def main():
    openai.api_key = os.environ["OPENAI_API_KEY"]
    openai.organization = os.environ["OPENAI_ORGANIZATION"]
    with open(prompt_file, "r") as f:
        prompt = f.read()

    expected_cmd = ["_build/default/bin/print_bench.exe"]
    if numeric:
        expected_cmd += ["-scaling", "1", "-numeric"]
    else:
        expected_cmd += ["-scaling", "2"]

    prompt += subprocess.check_output(
        expected_cmd,
        encoding="utf-8",
    )
    prompt += "\n"

    prompt += "Program:\n"
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {"role": "user", "content": prompt},
        ],
        max_tokens=100,
    )
    prog = response.choices[0]["message"]["content"].encode("utf-8").replace(b"`", b"")
    with open("full_prompt", "w") as f:
        f.write(prompt)
    print(prog)
    print(
        subprocess.check_output(
            ["_build/default/bin/print_bench.exe", "-scaling", "2"],
            input=prog,
        ).decode("utf-8")
    )


if __name__ == "__main__":
    args = docopt(__doc__)
    main(**args)
