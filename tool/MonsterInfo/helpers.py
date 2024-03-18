# -*- coding:utf-8 -*-

"""helpers.py"""
from pathlib import Path
import csv

__author__  = "Joel E Carlson"
__credits__ = ["joel.elmer.carlson@gmail.com"]
__email__   = __credits__[0]

PATH = Path.cwd()
MONSTER_INFO = "MonsterInfo"
MONSTERS_CSV = f"{PATH}/{MONSTER_INFO}/Monsters.csv"
TAG_CSV = f"{PATH}/{MONSTER_INFO}/MonstersTag.csv"

def _make_map(xs: object) -> dict:
    """
    _make_map unique
    """
    arr = {}
    for (k, v) in xs:
        arr[k] = v
    return arr

def _make_mons_tile(mon: str) -> str:
    """
    _make_mons_tile
    """
    tile = "MONS_" + mon.upper().replace("-", "_").replace(" ", "_")
    if mon == "draconian":
        tile = "BASE_DRACONIAN"
    elif mon == "black draconian":
        tile = "BASE_DRACONIAN_BLACK"
    elif mon == "yellow draconian":
        tile = "BASE_DRACONIAN_YELLOW"
    elif mon == "pale draconian":
        tile = "BASE_DRACONIAN_PALE"
    elif mon == "green draconian":
        tile = "BASE_DRACONIAN_GREEN"
    elif mon == "red draconian":
        tile = "BASE_DRACONIAN_RED"
    elif mon == "purple draconian":
        tile = "BASE_DRACONIAN_PURPLE"
    elif mon == "white draconian":
        tile = "BASE_DRACONIAN_WHITE"
    elif mon == "grey draconian":
        tile = "BASE_DRACONIAN_GREY"
    elif mon == "deep elf zephyrmancer":
        tile = "MONS_DEEP_ELF_AIR_MAGE"
    elif mon == "deep elf pyromancer":
        tile = "MONS_DEEP_ELF_FIRE_MAGE"
    elif mon == "shambling mangrove":
        tile = "MONS_TREANT"
    elif mon == "sickly merfolk siren":
        tile = "MONS_MERFOLK_SIREN_WATER"
    elif mon == "malarious merfolk avatar":
        tile = "MONS_MERFOLK_AVATAR_WATER"
    elif mon == "elephant slug":
        tile = "MONS_DART_SLUG"
    elif mon == "small abomination":
        tile = "MONS_ABOMINATION_SMALL"
    elif mon == "orphan":
        tile = "MONS_SIMULACRUM_EYE"
    elif mon == "mutant beast":
        tile = "MUTANT_BEAST_BASE"
    elif mon == "djinni":
        tile = "DJINNI_1"
    elif mon == "knight":
        tile = "MONS_ANCESTOR_KNIGHT"
    elif mon == "battlemage":
        tile = "MONS_ANCESTOR_BATTLEMAGE"
    elif mon == "hexer":
        tile = "MONS_ANCESTOR_HEXER"
    elif mon == "snaplasher vine":
        tile = "MONS_VINE_N"
    elif mon == "snaplasher vine segment":
        tile = "MONS_VINE_SEGMENT_N_S"
    elif mon == "large abomination":
        tile = "MONS_ABOMINATION_LARGE"
    elif mon == "starspawn tentacle":
        tile = "MONS_STARSPAWN_TENTACLE_N"
    elif mon == "starspawn tentacle segment":
        tile = "MONS_STARSPAWN_TENTACLE_SEGMENT_N_S"
    elif mon == "tentacle":
        tile = "MONS_KRAKEN_TENTACLE_WATER"
    elif mon == "tentacle segment":
        tile = "MONS_KRAKEN_TENTACLE_SEGMENT_WATER"
    elif mon == "eldritch tentacle":
        tile = "MONS_ELDRITCH_TENTACLE_N"
    elif mon == "eldritch tentacle segment":
        tile = "MONS_ELDRITCH_TENTACLE_SEGMENT_N_S"
    elif mon == "skeleton":
        tile = "MONS_SKELETON_MEDIUM"
    elif mon == "zombie":
        tile = "MONS_ZOMBIE_SMALL"
    elif mon == "simulacrum":
        tile = "MONS_SIMULACRUM_SMALL"
    elif mon == "spectral thing":
        tile = "MONS_SPECTRAL_SMALL"
    elif mon == "bound soul":
        tile = "MONS_LOST_SOUL"
    elif mon == "bone devil":
        tile = "MONS_RUST_DEVIL"
    elif mon == "orange crystal statue":
        tile = "MONS_ORANGE_STATUE"
    elif mon == "boulder":
        tile = "MONS_BOULDER_BEETLE_ROLLING"
    elif mon == "player":
        tile = "MONS_PLAYER_GHOST"
    elif mon == "Balor":
        tile = "MONS_ASMODEUS"
    elif mon == "Tarrasque":
        tile = "MONS_LERNAEAN_HYDRA_9"
    elif mon == "Witch-King":
        tile = "MONS_CEREBOV"
    elif mon == "Vampire King":
        tile = "MONS_ERESHKIGAL"
    else:
        tile
    return tile

def _merge_map(xs: dict, ys: list) -> dict:
    """
    _merge_map
    """
    for (k, v) in ys:
        try:
            current = xs[k]
            xs[k] = {**current, **v}
        except Exception as err:
            print (f"{err=}, {type(err)=}")
            raise
    return xs

def run():
    """
    run loadcsv
    """
    arr = []
    with open(MONSTERS_CSV, "r", encoding="UTF-8") as csvfile:
        reader = csv.DictReader(csvfile, delimiter=",", quotechar='"')
        for row in reader:
            name = row["Name"]
            arr.append ((name, row))

    tag = []
    with open(TAG_CSV, "r", encoding="UTF-8") as csvfile:
        reader = csv.DictReader(csvfile, delimiter=",", quotechar='"')
        for row in reader:
            name = row["Name"]
            row["Tile"] = _make_mons_tile(name)
            tag.append ((name, row))

    mons_map = _make_map(arr)
    merged   = _merge_map(mons_map, tag)
    mons = [(k,v) for (k,v) in merged.items()]
    return mons
