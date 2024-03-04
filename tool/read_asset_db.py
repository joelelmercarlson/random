# -*- coding:utf-8 -*-
"""
read_asset_db.py -- help with db
"""
import os
from pathlib import Path
import json
import DbInfo

PATH = Path.cwd()
ITEM_JSON = f"{PATH}/Items.json"
MONS_JSON = f"{PATH}/Monsters.json"

def _create_file (filename: str, xs: list):
    """
    create_file
    """
    print (f"Generating {filename}...")
    with open(filename, "w") as f:
        json_file = json.dumps(xs, sort_keys=True)
        f.write(json_file)
        f.close()

def run():
    """
    output Items.json, Monsters.json
    """
    db = DbInfo.DbInfo()
    os.chdir(PATH)
    _create_file(ITEM_JSON,db.item)
    _create_file(MONS_JSON,db.mons)

if __name__ == "__main__":
    run()
