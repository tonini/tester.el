(defun tester (&rest cl-keys)
  (cl--parsing-keywords ((:function nil) (:match nil)) nil
    (if (and buffer-file-name (string-match cl-match buffer-file-name))
        (setq tester--test-run-function cl-function)
      (setq tester--test-run-function nil))))

(defun tester--store-setup ()
  (setq tester--last-test-file buffer-file-name)
  (setq tester--last-test-function tester--test-run-function))

(defun tester--stored-setup-p ()
  (and tester--last-test-file
       tester--last-test-function))

(defun tester-run ()
  (interactive)
  (cond (tester--test-run-function
         (tester--store-setup)
         (funcall tester--test-run-function buffer-file-name))
        ((tester--stored-setup-p)
         (funcall tester--last-test-function tester--last-test-file))
        (t
         (message "Please setup a function for running tests."))))

(defun tonini-elixir-setup-hook ()
  (tester :function 'alchemist-mix-test-file :match "_test.exs"))

(add-hook 'elixir-mode-hook 'tonini-elixir-setup-hook)
