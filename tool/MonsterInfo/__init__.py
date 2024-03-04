# -*- coding:utf-8 -*-

""":class:`MonsterInfo`"""
from . import helpers

__author__  = "Joel E Carlson"
__credits__ = ["joel.elmer.carlson@gmail.com"]
__email__   = __credits__[0]

class MonsterInfo():
    """
    MonsterInfo

    :return: :class:`MonsterInfo`
    """
    def __init__(self):
        self.data    = helpers.run()
        self.current = 0
        self.length  = len(self.data)

    def __repr__(self):
        return (f"mons = {self.length}, ")

    def __iter__(self):
        return self

    def __next__(self):
        if self.current >= self.length:
            raise StopIteration
        self.current += 1
        try:
            item = self.data[self.current]
        except IndexError:
            item = ("dummy", mons_zero())
        return item

def mons_zero():
    """
    mons_zero
    """
    return {"Name": "dummy", "Tile": "MONS_PLAYER_GHOST"}
