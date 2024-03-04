# -*- coding:utf-8 -*-

"""helpers.py"""
from pathlib import Path
import csv

__author__  = "Joel E Carlson"
__credits__ = ["joel_carlson@optum.com"]
__email__   = __credits__[0]

PATH = Path.cwd()
ITEM_INFO = "ItemInfo"
ITEMS_CSV = f"{PATH}/{ITEM_INFO}/Items.csv"

def run():
    """
    run loadcsv
    """
    arr = []
    with open(ITEMS_CSV, "r", encoding="utf-8-sig") as csvfile:
        reader = csv.DictReader(csvfile, delimiter=",", quotechar='"')
        for row in reader:
            name = row["Name"]
            arr.append ((name, row))
    return arr
