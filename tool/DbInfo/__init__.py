# -*- coding:utf-8 -*-

""":class:`DbInfo`"""
from . import helpers

__author__  = "Joel E Carlson"
__credits__ = ["joel.elmer.carlson@gmail.com"]
__email__   = __credits__[0]

class DbInfo():
    """
    DbInfo

    :return: :class:`DbInfo`
    """
    def __init__(self):
        (self.item, self.mons) = helpers.run()
        self.data    = self.mons + self.item
        self.current = 0
        self.length  = len(self.data)

    def __repr__(self):
        return (f"db = {self.length}, ")

    def __iter__(self):
        return self

    def __next__(self):
        if self.current >= self.length:
            raise StopIteration
        self.current += 1
        try:
            item = self.data[self.current]
        except IndexError:
            item = []
        return item
