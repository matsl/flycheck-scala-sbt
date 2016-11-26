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

Mixed Java/Scala Projects
-------------------------

If you configure flycheck-scala-sbt for java-mode as well, add
`(flycheck-scala-sbt-init)` to your java-mode hook to set it up
properly as an on-save checker.

