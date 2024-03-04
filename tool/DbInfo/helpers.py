# -*- coding:utf-8 -*-

"""helpers.py"""
import glob
import json
import os
from pathlib import Path

__author__  = "Joel E Carlson"
__credits__ = ["joel.elmer.carlson@gmail.com"]
__email__   = __credits__[0]

PATH = Path.cwd()
DB_ROOT = f"{PATH}/db"
ITEM_ROOT = f"{DB_ROOT}/item"
MONS_ROOT = f"{DB_ROOT}/mons"

def _walk(dirname: str) -> list:
    """
    _walk
    """
    arr = []
    os.chdir(dirname)
    for f in glob.glob(f"**/*.json"):
        arr.append(f)
    final = [ dirname + "/" + f for f in arr ]
    return final

def run() -> (list,list):
    """
    run walk
    """
    mons_list = _walk(MONS_ROOT)
    mons = []
    for filename in mons_list:
        with open(filename, "r", encoding="utf-8-sig") as f:
            row = json.loads(f.read())
            name = row["Name"]
            mons.append((name, row))
            f.close()

    item_list = _walk(ITEM_ROOT)
    item = []
    for filename in item_list:
        with open(filename, "r", encoding="utf-8-sig") as f:
            row = json.loads(f.read())
            name = row["Name"]
            item.append((name, row))
            f.close()
    return (item, mons)
