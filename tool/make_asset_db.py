# -*- coding:utf-8 -*-
"""
make_asset_db.py -- help with Items.json and Monsters.json
"""
from pathlib import Path
import json
import os
import ItemInfo
import MonsterInfo
import TileInfo

PATH = Path.cwd()
DB_ROOT = f"{PATH}/db"
ITEM_ROOT = f"{DB_ROOT}/item"
MONS_ROOT = f"{DB_ROOT}/mons"

def _check_tile (xs: object, tile_map: dict) -> bool:
    """
    _check_tile
    """
    ok = True
    for (k, v) in xs:
        try:
            tile = v["Tile"]
            if len(tile) > 0:
                texcoord = tile_map[tile]
            else:
                pass
        except Exception as err:
            ok = False
            print (f"{err=}, {type(err)=} key={k}")
            raise
    return ok

def _check_tile1 (xs: object, tile_map: dict):
    """
    _check_tile1
    """
    for (k, v) in xs:
        try:
            tile = v["Tile1"]
            if len(tile) > 0:
                texcoord = tile_map[tile]
            else:
                pass
        except KeyError as err:
            print (f"{err=}, {type(err)=} key={k}")
            raise

def _create_db_file (filename: str, xs: object):
    """
    create_db_file
    """
    for (k,v) in xs.data:
        entry      = k.lower().replace(" ", "_").replace("-", "_")
        prefix     = _find_prefix(v)
        dirname    = f"{filename}/{prefix}"
        entry_file = f"{dirname}/{entry}.json"
        _create_dir(dirname)
        _create_file(entry_file, v)

def _create_dir(dirname: str):
    """
    create_dir
    """
    try:
        mode = 0o755
        os.mkdir(dirname, mode)
    except FileExistsError:
        pass

def _create_file (filename: str, xs: dict):
    """
    create_file
    """
    with open(filename, "w") as f:
        json_file = json.dumps(xs, sort_keys=True, indent=4)
        f.write(json_file)
        f.close()

def _find_prefix (xs: dict) -> str:
    """
    _find_prefix
    """
    item = "item/"
    mons = "mons/"
    try:
        prefix = item + xs["Skill"]
    except KeyError:
        prefix = item
    try:
        prefix = mons + xs["Base"]
    except KeyError:
        pass
    final = prefix.lower()
    return final

def _make_map(xs: object) -> dict:
    """
    _make_map unique
    """
    arr = {}
    for (k, v) in xs:
        arr[k] = v
    return arr

def run():
    """
    output db files
    """
    tiles = TileInfo.TileInfo()
    items = ItemInfo.ItemInfo()
    mons  = MonsterInfo.MonsterInfo()
    tile_map = _make_map(tiles)
    _check_tile(items, tile_map)
    _check_tile1(items, tile_map)
    _check_tile(mons, tile_map)
    _create_dir(DB_ROOT)
    _create_dir(ITEM_ROOT)
    _create_dir(MONS_ROOT)
    _create_db_file(DB_ROOT, items)
    _create_db_file(DB_ROOT, mons)

if __name__ == "__main__":
    run()
