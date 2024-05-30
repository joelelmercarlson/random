# INSTALL.md

RHEL/Centos requires building latest SDL2. Ubuntu is packaged.

## RHEL/CENTOS

## SDL2 from sources, libSDL
1. https://github.com/libsdl-org/SDL/
1. https://github.com/libsdl-org/SDL_image
1. https://github.com/libsdl-org/SDL_ttf

## /usr/local/lib
1. export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
1. export LD_LIBRARY_PATH=/usr/local/lib

## Ubuntu
1. sudo apt-get install libSDL2-dev libsdl2-ttf-dev libsdl2-image-dev

## MacOS
brew install make
brew install sdl2
brew install sdl2_image
brew install sdl2_ttf

## stack 
1. stack build
