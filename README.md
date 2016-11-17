flycheck-scala-sbt
==================

A [flycheck](http://www.flycheck.org/) checker for Scala which uses
[sbt-mode](https://github.com/ensime/emacs-sbt-mode) to provide a
persistent SBT session.

Usage
-----

Flycheck-scala-sbt is (or will shortly be) available on MELPA.  Call
`flycheck-scala-sbt-init` from your `scala-mode-hook`.

You'll probably want to make SBT files themselves NOT trigger this.
To do so, you can set up a special mode just for them derived from
scala-mode like this:

```elisp
(define-derived-mode sbt-conf-mode scala-mode "Sbt"
  "A mode for sbt configuration files.")

(add-to-list 'auto-mode-alist '("\\.sbt$" . sbt-conf-mode))
```
