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


(eval-and-compile
  (defun wlr-picture-to-text-and-offset (picture)
    "Parse a picture of the buffer with | representing point."
    (let ((parts (split-string picture "|")))
      (unless (eq 2 (length parts))
        (error "Picture of buffer contents should contain exactly one \"|\" placeholder to mark point"))
      (cons (apply 'concat parts)
            (+ 1 (length (car parts))))))
  (defun wlr-make-picture ()
    (concat (buffer-substring (point-min) (point)) "|"
            (buffer-substring (point) (point-max)))))


(defmacro wlr-before-after (before &rest steps)
  "Make a buffer look like BEFORE, then perform STEPS.
When a STEP is a string literal, it is assumed to be a picture of
the expected buffer contents (like BEFORE), and will be replaced
with a corresponding assertion on the buffer's current state."
  (pcase-let ((`(,initial-text . ,initial-offset) (wlr-picture-to-text-and-offset before)))
    `(with-temp-buffer
       (insert ,initial-text)
       (goto-char ,initial-offset)
       (setq-local comment-start "#")
       (whole-line-or-region-local-mode 1)
       ,@(mapcar (lambda (step)
                   (if (stringp step)
                       `(should (equal ,step (wlr-make-picture)))
                     step))
                 steps))))

(ert-deftest wlr-copy-whole-line-region-active ()
  (wlr-before-after
   "fir|st
second
third"
   (set-mark (point-min))
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (should (equal (current-kill 0) "fir"))
   ;; Should insert selected text before previous point
   (yank)
   "firfir|st
second
third"))

(ert-deftest wlr-copy-whole-line ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (should (equal (current-kill 0) "first\n"))
   ;; Should insert killed line before original line
   (yank)
   "first
fir|st
second
third"))

(ert-deftest wlr-copy-whole-line-when-readonly ()
  (wlr-before-after
   "fir|st
second
third"
   (read-only-mode 1)
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (should (equal (current-kill 0) "first\n"))   (forward-line 1)
   (call-interactively 'whole-line-or-region-copy-region-as-kill)
   (should (equal (current-kill 0) "second\n"))))

(ert-deftest wlr-yank-excludes-properties ()
  (wlr-before-after
   "st|
second
third"
   (let ((yank-excluded-properties (cons 'wlr-test-excluded yank-excluded-properties)))
     (save-excursion
       (goto-char (point-min))
       (insert (propertize "fir" 'wlr-test-excluded t)))
     (call-interactively 'whole-line-or-region-kill-ring-save)
     (should (equal (current-kill 0) "first\n"))
     ;; Should insert killed line before original line
     (yank)
     (should (not (get-text-property (- (point) 1) 'wlr-test-excluded))))
   "first
first|
second
third"))

(ert-deftest wlr-kill-region ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-kill-region)
   (should (equal (current-kill 0) "first\n"))
   "|second
third"
   (goto-char (point-max))
   ;; Should insert killed line before original line
   (yank)
   "second
first
third|"))

(ert-deftest wlr-yank-with-mode-disabled ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (should (equal (current-kill 0) "first\n"))
   (whole-line-or-region-local-mode -1)
   ;; Should insert killed line at point, not on previous line
   (yank)
   "firfirst
|st
second
third"))

(ert-deftest wlr-copy-several-whole-lines ()
  (wlr-before-after
   "fir|st
second
third"
   (let ((current-prefix-arg 2))
     (call-interactively 'whole-line-or-region-kill-ring-save))
   (should (equal (current-kill 0) "first\nsecond\n"))
   ;; Should insert killed lines before original line
   (yank)
   "first
second
fir|st
second
third"))

(ert-deftest wlr-copy-too-many-whole-lines ()
  (wlr-before-after
   "fir|st
second
third"
   (let ((current-prefix-arg 10))
     (call-interactively 'whole-line-or-region-kill-ring-save))
   (should (equal (current-kill 0) "first\nsecond\nthird"))
   ;; Should insert killed lines before original line
   (yank)
   "first
second
third
fir|st
second
third"))

(ert-deftest wlr-copy-whole-line-at-eof-with-no-eol ()
  (wlr-before-after
   "first
second
third|"
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (should (equal (current-kill 0) "third"))
   ;; Should insert killed line before original line
   (yank)
   "first
second
third
third|"))

(ert-deftest wlr-comment-dwim-region-active ()
  (wlr-before-after
   "fir|st
second
third"
   (set-mark (point-min))
   (call-interactively 'whole-line-or-region-comment-dwim)
   "# fir|
st
second
third"))

(ert-deftest wlr-comment-dwim-whole-line ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-comment-dwim)
   "# fir|st
second
third"))

(ert-deftest wlr-comment-dwim-prefix ()
  (wlr-before-after
   "fir|st
second
third"
   (let ((current-prefix-arg 2))
     (call-interactively 'whole-line-or-region-comment-dwim))
   "## fir|st
second
third"))

(ert-deftest wlr-comment-dwim-2-prefix ()
  (wlr-before-after
   "fir|st
second
third"
   (let ((current-prefix-arg 2))
     (call-interactively 'whole-line-or-region-comment-dwim-2))
   "# fir|st
# second
third"))

(ert-deftest wlr-comment-region ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-comment-region)
   "# fir|st
second
third"))

(ert-deftest wlr-comment-region-region-active ()
  (wlr-before-after
   "fir|st
second
third"
   (set-mark (point-min))
   (call-interactively 'whole-line-or-region-comment-region)
   "# fir|
st
second
third"))

(ert-deftest wlr-uncomment-region ()
  (wlr-before-after
   "# f|irst
second
third"
   (call-interactively 'whole-line-or-region-uncomment-region)
   "f|irst
second
third"))

(ert-deftest wlr-uncomment-region-region-active ()
  (wlr-before-after
   "# first
# second
|third"
   (set-mark (point-min))
   (call-interactively 'whole-line-or-region-uncomment-region)
   "first\nsecond\n|third"))

(ert-deftest wlr-rectangle-mark-still-works ()
  (wlr-before-after
   "|first
second
third"
   (rectangle-mark-mode 1)
   (forward-line 2)
   (forward-char 2)
   (call-interactively 'whole-line-or-region-copy-region-as-kill)
   (goto-char (point-min))
   (yank)
   "fifirst
sesecond
th|third"))


(provide 'whole-line-or-region-test)
;;; whole-line-or-region-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
