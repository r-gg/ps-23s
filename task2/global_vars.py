from typing import Dict, List

fns = {

    "mult": lambda x, y: x * y,
    "div": lambda x, y: x / y,
    "plus": lambda x, y: x + y,
    "add": lambda x, y: x + y,
    "minus": lambda x, y: x - y,
    # "cond" : lambda x, y, z: y if (x != 0) else z # TODO: cond logic should be implemented in evaluation, the only function that should be used is check cond
    "check_cond": lambda x: x != 0
}

# Environment for pairs
environment: Dict[str, Dict] = {}


def clean_env():
    global environment
    environment = {}
