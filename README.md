flycheck-scala-sbt
==================

A [flycheck](http://www.flycheck.org/) checker for Scala which uses
[sbt-mode](https://github.com/ensime/emacs-sbt-mode) to provide a
persistent SBT session.

Usage
-----

Flycheck-scala-sbt is available on MELPA.  The easy way to set it up
is to call `flycheck-scala-sbt-init` from your `scala-mode-hook`.
This just (locally) sets `flycheck-check-syntax-automatically` to
`(mode-enabled save)` and turns on flycheck-mode.
