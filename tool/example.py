# -*- coding:utf-8 -*-
"""
name_pairr.py -- help with crawl.png
"""
import TileInfo

def _file_label(tiles: object, ix: int) -> str:
    """
    file_label

    :return: filename
    """
    wall_max   = tiles.wall_len
    floor_max  = wall_max + tiles.floor_len
    feat_max   = floor_max + tiles.feat_len
    main_max   = feat_max + tiles.main_len
    player_max = main_max + tiles.player_len
    icons_max  = player_max + tiles.icons_len
    gui_max    = icons_max + tiles.gui_len

    if ix < wall_max:
        val = "wall.png"
    elif ix < floor_max:
        val = "floor.png"
    elif ix < feat_max:
        val = "feat.png"
    elif ix < main_max:
        val = "main.png"
    elif ix < player_max:
        val = "player.png"
    elif ix < icons_max:
        val = "icons.png"
    elif ix < gui_max:
        val = "gui.png"
    else:
        val = "tileinfo"
    return val


def run():
    tiles = TileInfo.TileInfo()
    hdr = ",".join(header)
    print (hdr)
    ix = 0
    for (k,v) in tiles:
        tex_coord = ",".join([ str(j) for j in v.values() ])
        label = _file_label(tiles,ix)
        print(f"{label},{ix},{k},{tex_coord}")
        ix += 1

header = [ "filename", "TID", "NAME", "WIDTH", "HEIGHT", "OX", "OY", "SX", "SY", "EX", "EY" ]

if __name__ == "__main__":
    run()
