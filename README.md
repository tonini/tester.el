# tester.el

> Run all tests through the same pipeline.

## Usage

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

To test the current file in the buffer, for example `person_management_spec.rb`, just run the function
`tester-run-test-file` and it will call the registered function `rspec-run-single-file`.

To run the whole test suite for the project, call `tester-run-test-suite`.
