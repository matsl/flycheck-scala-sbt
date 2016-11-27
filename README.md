flycheck-scala-sbt [![MELPA](https://melpa.org/packages/flycheck-scala-sbt-badge.svg)](https://melpa.org/#/flycheck-scala-sbt)
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

There are a small number of customizations available in the
`flycheck-scala-sbt` group to allow selecting various automatic
behaviors:

* Reloading the project automatically (default on)
* Exiting the REPL in order to run a check (default on)
* Displaying errors from other files in the current buffer (default
  on)
* Enabling in Java-mode if an SBT project root is discovered (default
  off)
* Initiating flychecks in other buffers open on the same project
  (somewhat experimental; default off)
* Creating a buffer containing all project errors that can be jumped
  to, like `flycheck-list-errors` but which can operate across files
  (default off)

Mixed Java/Scala Projects
-------------------------

If you configure flycheck-scala-sbt for java-mode as well, add
`(flycheck-scala-sbt-init)` to your java-mode hook to set it up
properly as an on-save checker.

Dealing with cross-file errors
------------------------------

Flycheck does not support errors in multiple files, but SBT is
intrinsically a whole-project compiler.  flycheck-scala-sbt provides a
couple of ways to deal with this.  First, customizing the
`flycheck-scala-sbt-collect-from-other-files-p` variable controls
whether errors from other files will be displayed (at the very top) of
the current buffer when a check is run.  In addition, if
`flycheck-scala-sbt-create-error-buffer-p` is true the whole list of
errors is placed in a buffer which can be accessed via the
`flycheck-scala-sbt-show-errors-list` command.
