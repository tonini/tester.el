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

;;; Code:

(defvar tester--storage (make-hash-table :test 'equal)
  "Hash table to store information about the last test run.")

(defun tester-init-test-run (function match)
  (puthash "match" match tester--storage)
  (if (and buffer-file-name (string-match match buffer-file-name))
      (puthash "test-run-function" function tester--storage)
    (puthash "test-run-function" nil tester--storage)))

(defun tester-init-test-suite-run (function)
  (puthash "test-suite-run-function" function tester--storage))

(defun tester--store-test-file-run ()
  (puthash "last-test-file" buffer-file-name tester--storage)
  (puthash "last-test-function" (gethash "test-run-function" tester--storage) tester--storage))

(defun tester--store-test-file-run-p ()
  (and (gethash "last-test-file" tester--storage)
       (gethash "last-test-function" tester--storage)))

(defun tester--test-run-function ()
  (gethash "test-run-function" tester--storage))

(defun tester--test-suite-run-function ()
  (gethash "test-suite-run-function" tester--storage))

(defun tester--last-test-function ()
  (gethash "last-test-function" tester--storage))

(defun tester--last-test-suite-function ()
  (gethash "last-test-suite-function" tester--storage))

(defun tester--last-test-file ()
  (gethash "last-test-file" tester--storage))

(defun tester--buffer-test-file-p ()
  (and buffer-file-name
       (string-match (gethash "match" tester--storage) buffer-file-name)))

(defun tester-run-test-file ()
  (interactive)
  (cond ((and (tester--test-run-function)
              (tester--buffer-test-file-p))
         (tester--store-test-file-run)
         (funcall (tester--test-run-function) buffer-file-name))
        ((tester--store-test-file-run-p)
         (funcall (tester--last-test-function) (tester--last-test-file)))
        (t
         (message "Please setup a function for running a test."))))

(defun tester-run-test-suite ()
  (interactive)
  (cond ((tester--test-suite-run-function)
         (puthash "last-test-suite-function" (tester--test-suite-run-function) tester--storage)
         (funcall (tester--test-suite-run-function)))
        ((tester--last-test-suite-function)
         (funcall (tester--last-test-suite-function)))
        (t
         (message "Please setup a function for running the test suite."))))

(provide 'tester)

;;; tester.el ends here
