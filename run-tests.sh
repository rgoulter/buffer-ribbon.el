#!/usr/bin/env bash
emacs -batch \
      -l ert \
      -l dev/bootstrap.el \
      -l buffer-ribbon.el \
      -l test/test-helper.el \
      -l test/buffer-ribbon-test.el \
      -f ert-run-tests-batch-and-exit
