# -*- coding:utf-8 -*-
"""
make_glyph.py -- help with crawl.png
"""
import json
import TileInfo

def _create_file (filename: str, xs: dict):
    """
    create_file
    """
    print (f"Generating {filename}...")
    with open(filename, "w") as f:
        json_file = json.dumps(xs.data, sort_keys=True)
        f.write(json_file)
        f.close()

def run():
    """
    output Glyph.json
    """
    tiles = TileInfo.TileInfo()
    _create_file("Glyphs.json", tiles)

if __name__ == "__main__":
    run()
