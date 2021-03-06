*buffer-ribbon.el*: Emacs support for buffer ribbons and patch grids.

[![Build Status](https://travis-ci.com/rgoulter/buffer-ribbon.el.svg?branch=master)](https://travis-ci.com/rgoulter/buffer-ribbon.el)
[![Coverage Status](https://coveralls.io/repos/github/rgoulter/buffer-ribbon.el/badge.svg?branch=master)](https://coveralls.io/github/rgoulter/buffer-ribbon.el?branch=master)

As inspired by A. Henley's dissertation
[Human Centric tools for Navigating Code](http://web.eecs.utk.edu/~azh/pubs/Henley2018bDissertation.pdf).  
See also: [short video demonstrating Patchworks Code
Editor](https://www.youtube.com/watch?v=GwcxDZT3pXE).

A _buffer ribbon_ is a strip of columns of buffers.

A _patch grid_ is a 3x2 tiled grid of windows,
which acts as a scrollable viewport of a buffer ribbon.

It's similar to tiled desktops, with a continuous column
of tiles.

## Visualisation / Screenshots

On my admittedly crammed laptop screen, the effect is:

#### Scrolling

![](screenshots/scroll-buffer-ribbon.gif)

_A patch grid already filled with buffers._
_The animation shows the patch grid 'scrolling' back_
_and forth._

#### "Zoom" Into Grid Tile

![](screenshots/zoom-buffer-ribbon.gif)

_A patch grid already filled with buffers._
_The animation shows the patch grid 'zooming'_
_into one of the grid tiles._

#### Scrolling and then "Zoom" Into Grid Tile

![](screenshots/scroll-and-zoom-buffer-ribbon.gif)

_A patch grid already filled with buffers._
_The animation shows using the patch grid to_
_scroll and then zoom into one of the tiles._

## Command Overview

- `buffer-ribbon/init-patch-grid-using-selected-window`:
  sets up a patch grid using the currently selected window.
  - `buffer-ribbon/init-patch-grid-using-selected-window`:
    opens a new frame and uses that for the patch grid.

- `buffer-ribbon/scroll-patch-grid-left` / `buffer-ribbon/scroll-buffer-ribbon-right`:
  scrolls to show the buffers to the left of the patch grid.

- `buffer-ribbon/scroll-patch-grid-right` / `buffer-ribbon/scroll-buffer-ribbon-left`
  scrolls to show the buffers to the right of the patch grid.

- `buffer-ribbon/zoom-selected-window`:
  "zooms in" to the selected window so that it takes up
  the whole frame.
  - `buffer-ribbon/unzoom`: zooms out so that
    the window configuration is as it was before
    zooming int.

## Sample Hydra

The commands are more useful bound to a keymap.
e.g. using [abo-abo/hydra](https://github.com/abo-abo/hydra):

```emacs-lisp
(defhydra hydra-patch-grid (global-map "C-c r")
  "patch-grid"
  ("h" evil-window-left  "select left")
  ("j" evil-window-down  "select down")
  ("k" evil-window-up    "select up")
  ("l" evil-window-right "select right")

  ("H" buffer-ribbon/scroll-patch-grid-left "shift left")
  ("L" buffer-ribbon/scroll-patch-grid-right "shift right")

  ("J" buffer-ribbon/zoom-selected-window "zoom in" :exit t)
  ("K" buffer-ribbon/unzoom "zoom out")

  ("b" switch-to-buffer "switch buffer")

  ("o" other-window "cycle to other-window")

  ("gw"
   (buffer-ribbon/select-patch-grid-window 0 0)
   "jump to grid tile 0, 0")
  ("ge"
   (buffer-ribbon/select-patch-grid-window 1 0)
   "jump to grid tile 1, 0")
  ("gr"
   (buffer-ribbon/select-patch-grid-window 2 0)
   "jump to grid tile 2, 0")
  ("gs"
   (buffer-ribbon/select-patch-grid-window 0 1)
   "jump to grid tile 0, 1")
  ("gd"
   (buffer-ribbon/select-patch-grid-window 1 1)
   "jump to grid tile 1, 1")
  ("gf"
   (buffer-ribbon/select-patch-grid-window 2 1)
   "jump to grid tile 2, 1"))
```

## Interaction with Other Packages

#### org-mode

``` emacs-lisp
(setq org-agenda-sticky t)  ; don't delete buffer after quitting
(setq org-agenda-window-setup 'current-window)  ; use current-window for org agenda
```

## Principles for Navigating within Working Sets

My understanding of section 3.6.4 of [Human Centric tools for
Navigating
Code](http://web.eecs.utk.edu/~azh/pubs/Henley2018bDissertation.pdf),
which outlines guiding principles for navigation efficiency:

- Juxtaposition Principle: If the working set of code is on
  the screen (e.g. juxtaposed using tiles), the user doesn't
  have to navigate outside of that.
- Signpost Principle: It's good for the editor to provide
  'signposts' for good navigation decisions.
  - Negative Example: Emacs' Buffer List doesn't provide
    good signposts which help a programmer remember
    which method was where, etc.
  - Example: Patchworks Code Editor's ribbon view showed
    the shape of the edited code, which assisted
    in navigating to the tile.
  - Not yet implemented in buffer-ribbon.el
- Blowup Principle: Since the tiles may be too small,
  it's useful to be able to zoom into the patch
  (and zoom back out to the patch grid).
- Cleanup Principle: make it easy to retain relevant 'patches'
  of code, and easy to get rid of irrelavent 'patches'.
  - Not particularly addressed in buffer-ribbon.el
- Heterogeneity Principle: Patch grid tiles can show things
    other than code.
  - e.g. org agenda buffers, magit status,
    compilation output, shell.
