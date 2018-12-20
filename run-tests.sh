#!/usr/bin/env bash
emacs -batch \
      -l ert \
      -l dev/bootstrap.el \
      -l buffer-ribbon.el \
      -l tests/buffer-ribbon-tests.el \
      -f ert-run-tests-batch-and-exit
