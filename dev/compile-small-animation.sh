#!/usr/bin/env bash

set -ex

IMAGES=(
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-2.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-select.png
../screenshots/large/buffer-ribbon-zoom.png
)

SCROLL_ANIM="
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-2.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-scroll-2.png
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-1.png
"

ZOOM_ANIM="
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-select.png
../screenshots/large/buffer-ribbon-zoom.png
"

SCROLL_AND_ZOOM_ANIM="
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-2.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-select.png
../screenshots/large/buffer-ribbon-select.png
../screenshots/large/buffer-ribbon-zoom.png
../screenshots/large/buffer-ribbon-zoom.png
../screenshots/large/buffer-ribbon-zoom.png
../screenshots/large/buffer-ribbon-select.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-scroll-3.png
../screenshots/large/buffer-ribbon-scroll-2.png
../screenshots/large/buffer-ribbon-scroll-1.png
../screenshots/large/buffer-ribbon-scroll-1.png
"

convert -delay 100 -scale 50% -loop 0 ${SCROLL_ANIM} ../screenshots/scroll-buffer-ribbon.gif
convert -delay 300 -scale 50% -loop 0 ${ZOOM_ANIM} ../screenshots/zoom-buffer-ribbon.gif
convert -delay 100 -scale 50% -loop 0 ${SCROLL_AND_ZOOM_ANIM} ../screenshots/scroll-and-zoom-buffer-ribbon.gif
