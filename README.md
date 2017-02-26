flycheck-scala-sbt [![MELPA](https://melpa.org/packages/flycheck-scala-sbt-badge.svg)](https://melpa.org/#/flycheck-scala-sbt)
==================

A [flycheck](http://www.flycheck.org/) checker for Scala which uses
[sbt-mode](https://github.com/ensime/emacs-sbt-mode) to provide a
persistent SBT session.

Usage
-----

Flycheck-scala-sbt is available on MELPA.  The easy way to set it up
is to call `flycheck-scala-sbt-init` from your `scala-mode-hook` and
optionally `java-mode-hook`.  This just (locally) sets
`flycheck-check-syntax-automatically` to `(mode-enabled save)` and
turns on flycheck-mode.

There are a small number of customizations available in the
`flycheck-scala-sbt` group to allow selecting various automatic
behaviors.

* Reloading the project automatically when the project itself is
  changed (default on)
* Exiting an `sbt console` repl in order to issue a compile command
  (default on)
* Creating a buffer containing all project errors that can be jumped
  to, like `flycheck-list-errors` but which can operate across files
  (default on)
* Registering as a Java-mode checker if an SBT project root is
  discovered (default off)

Mixed Java/Scala Projects
-------------------------

If you configure flycheck-scala-sbt for java-mode as well, add
`(flycheck-scala-sbt-init)` to your java-mode hook to set it up
properly as an on-save checker.

Dealing with cross-file errors
------------------------------

Flycheck does not support errors in multiple files, but SBT is
intrinsically a whole-project compiler.  flycheck-scala-sbt will
collect errors from other files and put them in a diagnostic located
at the top of the current buffer.  In addition, the whole list of
errors is placed in a buffer which can be accessed via the
`flycheck-scala-sbt-show-errors-list` command.
