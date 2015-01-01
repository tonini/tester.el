;;; tester.el --- Test interface for language specific test runs.

;; Copyright © 2015 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; URL: http://www.github.com/tonini/tester.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (pkg-info "0.4"))
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Test interface for language specific test runs.

;;
;;  Example usage:
;;
;;  (defun custom-elixir-setup-hook ()
;;    (tester :function 'alchemist-mix-test-file :match "_test.exs"))
;;
;;  (add-hook 'elixir-mode-hook 'custom-elixir-setup-hook)
;;

;;; Code:

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

(provide 'tester)

;;; tester.el ends here
