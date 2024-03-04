# -*- coding:utf-8 -*-

""":class:`TileInfo`"""
from . import tileinfo_wall
from . import tileinfo_floor
from . import tileinfo_feat
from . import tileinfo_main
from . import tileinfo_player
from . import tileinfo_icons
from . import tileinfo_gui

__author__  = "Joel E Carlson"
__credits__ = ["joel_carlson@optum.com"]
__email__   = __credits__[0]

class TileInfo():
    """
    TileInfo

    :return: :class:`TileInfo`
    """
    def __init__(self):
        self.wall   = tileinfo_wall.run()
        self.floor  = tileinfo_floor.run()
        self.feat   = tileinfo_feat.run()
        self.main   = tileinfo_main.run()
        self.player = tileinfo_player.run()
        self.icons  = tileinfo_icons.run()
        self.gui    = tileinfo_gui.run()
        self.data = self.wall + self.floor + self.feat + self.main + self.player + self.icons + self.gui
        self.current    = 0
        self.length     = len(self.data)
        self.wall_len   = len(self.wall)
        self.floor_len  = len(self.floor)
        self.feat_len   = len(self.feat)
        self.main_len   = len(self.main)
        self.player_len = len(self.player)
        self.icons_len  = len(self.icons)
        self.gui_len    = len(self.gui)

    def __repr__(self):
        return (f"tiles = {self.length}, "
                f"wall = {self.wall_len}, "
                f"floor = {self.floor_len}, "
                f"feat = {self.feat_len}, "
                f"main = {self.main_len}, "
                f"player = {self.player_len}, "
                f"icons = {self.icons_len}, "
                f"gui = {self.gui_len}, "
                )

    def __iter__(self):
        return self

    def __next__(self):
        if self.current >= self.length:
            raise StopIteration
        self.current += 1
        try:
            tile = self.data[self.current]
        except IndexError:
            tile = ("NOT_TILE", tile_zero())
        return tile

def tile_zero():
    """
    tile_zero - 1st texcoord
    """
    return {"w": 32, "h": 32, "ox":0, "oy":32, "sx":0, "sy":0, "ex":32, "ey":32}
