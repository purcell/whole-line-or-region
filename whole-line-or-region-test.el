;;; whole-line-or-region-test.el --- Tests for whole-line-or-region functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for whole-line-or-region

;;; Code:

(require 'ert)
(require 'whole-line-or-region)


(defmacro wlr-simple (text &rest body)
  "Execute BODY in a temp buffer into which TEXT has been inserted."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (whole-line-or-region-mode 1)
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(ert-deftest wlr-copy-whole-line-region-active ()
  (wlr-simple "first\nsecond\nthird"
    (call-interactively 'set-mark-command)
    (forward-char 3)
    (call-interactively 'whole-line-or-region-copy-region-as-kill)
    (should (equal (current-kill 0) "fir"))
    ;; Should insert selected text before previous point
    (call-interactively 'whole-line-or-region-yank)
    (should (equal "firfirst\nsecond\nthird" (buffer-string)))
    (should (looking-back "firfir"))))

(ert-deftest wlr-copy-whole-line ()
  (wlr-simple "first\nsecond\nthird"
    (forward-char 3)
    (call-interactively 'whole-line-or-region-copy-region-as-kill)
    (should (equal (current-kill 0) "first\n"))
    ;; Should insert killed line before original line
    (call-interactively 'whole-line-or-region-yank)
    (should (equal "first\nfirst\nsecond\nthird" (buffer-string)))
    (should (eq (point) (+ (point-min) 3 (length "first\n"))))))

(ert-deftest wlr-copy-several-whole-lines ()
  (wlr-simple "first\nsecond\nthird"
    (forward-char 3)
    (let ((current-prefix-arg 2))
      (call-interactively 'whole-line-or-region-copy-region-as-kill))
    (should (equal (current-kill 0) "first\nsecond\n"))
    ;; Should insert killed lines before original line
    (call-interactively 'whole-line-or-region-yank)
    (should (equal "first\nsecond\nfirst\nsecond\nthird" (buffer-string)))
    (should (eq (point) (+ (point-min) 3 (length "first\nsecond\n"))))))

(ert-deftest wlr-copy-whole-line-at-eof-with-no-eol ()
  (wlr-simple "first\nsecond\nthird"
    (goto-char (point-max))
    (call-interactively 'whole-line-or-region-copy-region-as-kill)
    (should (equal (current-kill 0) "third"))
    ;; Should insert killed line before original line
    (call-interactively 'whole-line-or-region-yank)
    (should (equal "first\nsecond\nthird\nthird" (buffer-string)))
    ;; Currently fails:
    ;;(should (eq (point) (point-max)))
    ))

(provide 'whole-line-or-region-test)
;;; whole-line-or-region-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
