#!/bin/bash
# Convert all SVGs to PNGs

shopt -s nullglob
for svg in *.svg
do
    width=$(identify -format "%w" $svg)
    png="$(basename -s .svg $svg).png"
    inkscape $svg -e $png -w $width
done
