[![Melpa Status](http://melpa.org/packages/flycheck-package-badge.svg)](http://melpa.org/#/flycheck-package)
[![Melpa Stable Status](http://stable.melpa.org/packages/flycheck-package-badge.svg)](http://stable.melpa.org/#/flycheck-package)

flycheck-package
===============

This library provides a [flycheck][] checker for the metadata in
Emacs Lisp files which are intended to be packages.

Currently these checks are only activated if a `Package-Requires`
header is present in the file, and checks center on the validity of
the data in that header.

Installation
------------

You'll need Emacs 24 for `flycheck`, so the recommended way to get
`flycheck-package` is as a package from the [MELPA][melpa]
repository. The version of `flycheck-package` there will always be
up-to-date. There are also packages in [MELPA Stable][melpa-stable], which
track the [latest numbered tag][tags].

If you insist on doing things the hard way, first ensure `flycheck` is
installed, then download this code and add the directory to your Emacs
`load-path`.

Then, in your `init.el`:

```lisp
(eval-after-load 'flycheck
  '(flycheck-package-setup))
```

Roadmap
-------

Once this is more useful, the plan is to extract the checks into a separate
`package-lint.el`, which can then be used by this checker and also by
MELPA -- this will probably be essential in order to check multi-file packages
helpfully anyway.

Checks to add in the short term:

- WARN: Required version numbers of packages should be available for installation
- WARN: Stable version numbers should be used in dependencies when possible

Further checks for a future package-lint.el, some of which are currently
performed by other flycheck checkers:

- WARN: header line formatting / length / capitalisation / use of "Emacs"
- trailing line presence / formatting
- URL header presence
- WARN: suggest cl-lib vs cl
- ERROR: don't depend on (cl-lib "1.0")
- keywords separated by only spaces
- use of non-standard keywords
- non-use of standard keywords
- checkdoc failures for interactive functions / defcustoms
- presence of :group for each defcustom / defgroup
- trailing whitespace?
- themes which aren't in a matching *-theme.el file
- files lacking a (provide ...) which matches their name
- use of unsafe local variables
- use of emacs version dependencies
- `lexical-binding: t` set on a line other than the first
- local variable set in header line
- non-empty commentary
- using commentary to talk about load[- ]path and installation
- lack of autoloads
- references to README files which won't be bundled in a package
- dependencies on unavailable packages / versions
- use of CamelCase identifiers

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See
[COPYING](https://github.com/purcell/flycheck-package/blob/master/COPYING)
for details.

About
-----

`flycheck-package` was written by [Steve Purcell](https://github.com/purcell).

<hr>

Author links:

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)

[flycheck]: https://github.com/flycheck/flycheck
[tags]: https://github.com/purcell/flycheck-package/tags
[ledger]: https://ledger-cli.org/
[melpa-stable]: http://stable.melpa.org
[melpa]: http://melpa.org
