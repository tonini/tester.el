# tester.el

> Run all tests through the same pipeline.

`tester` gives you the ability to always use the same two functions to run a test file or the whole test suite and that in every language major mode you like.

All what you have to do is to explain `tester` what in which context have to be called when you call the functions.

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

### Setup

There are two functions available to register the functions which should be executed at test time, `tester-init-test-run` and `tester-init-test-suite-run`.

* `tester-init-test-run`

Takes two arguments, the function which will be executed at test time and the identifier for a test file.

* `tester-init-test-suite-run`

Takes just one argument, the function which will used to run the complete project based test suite.
