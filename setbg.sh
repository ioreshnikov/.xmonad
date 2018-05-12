#!/bin/bash


input=$1;
output=$HOME/.background.png;


convert "$input" \
  -resize 1920x1080^ -gravity center -extent 1920x1080 \
  \( +clone -fx '(h-j) > 72 ? 1.0 : ((0.1 - 1) * j + h - 0.1 * (h - 72))/72' \) \
  -compose multiply \
  -composite \
  $output;

feh --bg-fill $output;
