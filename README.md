[![Melpa Status](http://melpa.org/packages/package-lint-badge.svg)](http://melpa.org/#/package-lint)
[![Melpa Stable Status](http://stable.melpa.org/packages/package-lint-badge.svg)](http://stable.melpa.org/#/package-lint)
[![Build Status](https://github.com/purcell/package-lint/workflows/CI/badge.svg)](https://github.com/purcell/package-lint/actions)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

package-lint
============

This library provides a linter for the metadata in Emacs Lisp files
which are intended to be packages. You can integrate it into your
build process.

`package-lint` detects various issues that may make your package
uninstallable or unusable for some users, and it warns about
significant deviations from the [Elisp coding
conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html),
such as non-compliant symbol naming, and use of reserved
keybindings. Among other community uses, `package-lint` is a
prerequisite for submission of packages to MELPA.

`package-lint` can be used standalone, but see also the
[flycheck-package](https://github.com/purcell/flycheck-package) and
`package-lint-flymake` packages, which both use `package-lint` to
conveniently display packaging errors directly in the buffer while
writing elisp packages.

Installation
------------

The recommended way to get
`package-lint` is as a package from the [MELPA][melpa]
repository. The version of `package-lint` there will always be
up-to-date. There are also packages in [MELPA Stable][melpa-stable], which
track the [latest numbered tag][tags].

Usage
-----

Use the command `package-lint-current-buffer` interactively, or use
`package-lint-buffer` if linting programmatically.

If you're a package author, you can include `package-lint` in your
build process by ensuring that the package is installed, and then
using the function `package-lint-batch-and-exit` to lint your files --
see `run-tests.sh` in this repo for an example.


Roadmap
-------

Additional checks for future versions:

- WARN: header line formatting / capitalisation
- trailing line presence / formatting
- WARN: library is called *-mode but doesn't provide a major mode
- checkdoc failures for interactive functions / defcustoms
- trailing whitespace?
- themes which aren't in a matching *-theme.el file
- use of unsafe local variables
- local variable set in header line
- using commentary to talk about load[- ]path and installation
- lack of autoloads
- references to README files which won't be bundled in a package
- use of CamelCase identifiers
- Defining a `-mode` function directly instead of using `define-derived-mode` or `define-minor-mode`
- Referring to `display-graphic-p` or `window-system` in `-theme.el` files

License
-------

Please refer to the file `LICENSE`.

Credits
-------

`package-lint` was written by
[Steve Purcell](https://github.com/purcell) with significant
contributions from [Fanael Linithien](https://github.com/Fanael).

<hr>

Author links:

[💝 Support this project and my other Open Source work](https://www.patreon.com/sanityinc)

[💼 LinkedIn profile](https://uk.linkedin.com/in/stevepurcell)

[✍ sanityinc.com](http://www.sanityinc.com/)

[🐦 @sanityinc](https://twitter.com/sanityinc)

[✍ fanael.github.io](https://fanael.github.io/)

[flycheck]: https://github.com/flycheck/flycheck
[tags]: https://github.com/purcell/flycheck-package/tags
[ledger]: https://ledger-cli.org/
[melpa-stable]: http://stable.melpa.org
[melpa]: http://melpa.org
