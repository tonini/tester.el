[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

# tester.el

> Run all tests through the same pipeline.

`tester` gives you the ability to always use the same two functions to run a test file or the whole test suite and that in every language major mode you like.

All what you have to do is to explain `tester` what in which context have to be called when you call the functions.

***

- [Installation](#installation)
  - [ELPA](#installation-via-packageel)
  - [Manual](#manual)
- [Usage](#usage)
  - [Setup](#setup)
  - [Examples](#examples)
  - [Additional functionality](#additional-functionality)
- [Contributing](#contributing)
- [License](#license)

## Installation

### Installation via package.el

`package.el` is the built-in package manager in Emacs.

Tester is available on the three major community maintained repositories -
[MELPA STABLE](melpa-stable.milkbox.net), [MELPA](http://melpa.milkbox.net) and [Marmalade](https://marmalade-repo.org/).

You can install `Tester` with the following commnad:

<kbd>M-x package-install [RET] tester [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'tester)
  (package-install 'tester))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining Tester, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

With the most recent builds of Emacs, you can pin Tester to always
use MELPA Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(tester . "melpa-stable") t)
```

### Manual

You can install Tester manually by placing it on your `load-path` and
`require` ing it. Many people favour the folder `~/.emacs.d/vendor`.

```el
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'tester)
```

## Usage

### Setup

There are two functions available to register the functions which should be executed at test time, `tester-init-test-run` and `tester-init-test-suite-run`.

* `tester-init-test-run`

Takes two arguments, the function which will be executed at test time and the identifier for a test file.

* `tester-init-test-suite-run`

Takes just one argument, the function which will used to run the complete project based test suite.

After you registered the functions you can just call `tester-run-test-file`. When you're in a prober buffer with a test file, the registered function will be called.

The function `tester-run-test-suite` can be called from everywhere inside your project, you don't need to be in a buffer with a proper test file.

### Examples

Below you see a few examples how you setup `tester` for different language major modes.

* Elixir Example:

```el
(defun default-elixir-mode-hook ()
  (tester-init-test-run #'alchemist-mix-test-file "_test.exs$")
  (tester-init-test-suite-run #'alchemist-mix-test))

(add-hook 'elixir-mode-hook  'default-elixir-mode-hook)
```

* Ruby Example:

```el
(defun default-ruby-mode-hook ()
  (tester-init-test-run #'rspec-run-single-file "_spec.rb$")
  (tester-init-test-suite-run #'rake-test))

(add-hook 'ruby-mode-hook  'default-ruby-mode-hook)
```

* ELisp Example:

```el
(defun default-emacs-lisp-mode-hook ()
  (tester-init-test-run #'overseer-test-file "test.el$")
  (tester-init-test-suite-run #'overseer-test))

(add-hook 'emacs-lisp-mode-hook  'default-emacs-lisp-mode-hook)
```

### Additional functionality

`tester` not just run your registered functions, it also stores them. So if you try to call for example `tester-run-test-file` in a buffer with doesn't contain a proper test file, it just calls
the last stored test run. This brings a useful workflow, for example you can switch between a test file and the proper codebase and run the tests from both buffers.

## Contributing

Contributions are very welcome!

1. Fork tester.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

## License

Copyright © 2015 Samuel Tonini and
[contributors](https://github.com/tonini/tester.el/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?style=flat
