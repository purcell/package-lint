[![Melpa Status](http://melpa.org/packages/package-lint-badge.svg)](http://melpa.org/#/package-lint)
[![Melpa Stable Status](http://stable.melpa.org/packages/package-lint-badge.svg)](http://stable.melpa.org/#/package-lint)
[![Build Status](https://travis-ci.org/purcell/package-lint.png?branch=master)](https://travis-ci.org/purcell/package-lint)

package-lint
============

This library provides a linter for the metadata in Emacs Lisp files
which are intended to be packages.

Currently these checks are only activated if a `Package-Requires` or
`Package-Version` header is present in the file, or if non-nil `force`
is passed to `package-lint-buffer`, and checks center on the validity of
the data in that header.

See [flycheck-package](https://github.com/purcell/flycheck-package),
which uses this code to conveniently display packaging errors while
writing elisp packages. This code was extracted from
`flycheck-package.el`. We eventually want to use it for MELPA -- it
will need to learn how to check multi-file packages, which was always
going to be out of scope for `flycheck-package`.


Installation
------------

The recommended way to get
`package-lint` is as a package from the [MELPA][melpa]
repository. The version of `package-lint` there will always be
up-to-date. There are also packages in [MELPA Stable][melpa-stable], which
track the [latest numbered tag][tags].

Roadmap
-------

Additional checks for future versions:

- WARN: header line formatting / capitalisation
- trailing line presence / formatting
- URL header presence
- URL header value is a single valid http(s) URL
- WARN: library is called *-mode but doesn't provide a major mode
- WARN: suggest cl-lib vs cl
- non-use of standard keywords
- checkdoc failures for interactive functions / defcustoms
- presence of :group for each defcustom / defgroup
- trailing whitespace?
- themes which aren't in a matching *-theme.el file
- files lacking a (provide ...) which matches their name
- use of unsafe local variables
- use of emacs version dependencies
- local variable set in header line
- use of nadvice.el without depending on Emacs 24.4
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

Credits
-------

`package-lint` was written by
[Steve Purcell](https://github.com/purcell) with significant
contributions from [Fanael Linithien](https://github.com/Fanael).

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
