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

;; Reset all the tests for this package, particularly to remove
;; deleted or renamed tests
(cl-loop for s being the symbols
         do (when (string-prefix-p "wlr-" (symbol-name s))
              (ert-delete-test s)))


;;; Express tests in terms of simple "pictures" of the buffer contents

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
            (buffer-substring (point) (point-max))))
  (defun wlr-apply-picture (picture)
    (pcase-let ((`(,text . ,offset) (wlr-picture-to-text-and-offset picture)))
      (delete-region (point-min) (point-max))
      (insert text)
      (goto-char offset))))

(ert-deftest wlr-pictures ()
  (with-temp-buffer
    (wlr-apply-picture "foo|bar\n")
    (should (equal "foobar\n" (buffer-string)))
    (should (eq 4 (point)))
    (forward-char 2)
    (should (equal "fooba|r\n" (wlr-make-picture)))))

(defmacro wlr-with-pictures (before &rest steps)
  "Make a buffer look like BEFORE, then perform STEPS.
When a STEP is a string literal, it is assumed to be a picture of
the expected buffer contents (like BEFORE), and will be replaced
with a corresponding assertion on the buffer's current state."
  `(progn
     (wlr-apply-picture ,before)
     ,@(mapcar (lambda (step)
                 (if (stringp step)
                     `(should (equal ,step (wlr-make-picture)))
                   step))
               steps)))


;;;

(defmacro wlr-before-after (before &rest steps)
  "Run `wlr-ert-with-pictures' for BEFORE and STEPS in a temp buffer."
  `(with-temp-buffer
     (let (kill-ring)
       (setq-local comment-start "#")
       (whole-line-or-region-local-mode 1)
       (wlr-with-pictures ,before ,@steps))))

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
   (should (eq (point-min) (mark)))
   "first
fir|st
second
third"))

(ert-deftest wlr-yank-in-empty-buffer ()
  (wlr-before-after
   "fir|st"
   (call-interactively 'whole-line-or-region-kill-region)
   "|"
   (yank)
   "first
|"))

(ert-deftest wlr-yank-with-newline-in-empty-buffer ()
  (wlr-before-after
   "fir|st\n"
   (call-interactively 'whole-line-or-region-kill-region)
   "|"
   (yank)
   "first
|"))

(ert-deftest wlr-yank-in-empty-buffer-with-newline ()
  (wlr-before-after
   "fir|st"
   (call-interactively 'whole-line-or-region-kill-region)
   "|"
   (newline)
   "
|"
   (yank)
   "
first
|"))

(ert-deftest wlr-kill-region-preserves-column ()
  (wlr-before-after
   "fir|st
second"
   (call-interactively 'whole-line-or-region-kill-region)
   "sec|ond"))

(ert-deftest wlr-copy-whole-line-bol ()
  (wlr-before-after
   "|first
second"
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (should (equal (current-kill 0) "first\n"))
   (yank)
   (should (eq (point-min) (mark)))
   "first
|first
second"))

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

(ert-deftest wlr-honours-kill-read-only-ok ()
  (wlr-before-after
   "fir|st
second
third"
   (read-only-mode 1)
   (should-error (call-interactively 'whole-line-or-region-kill-region))
   (let ((kill-read-only-ok t))
     (call-interactively 'whole-line-or-region-kill-region)
     (should (equal (current-kill 0) "first\n")))
   "fir|st
second
third"))

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

(ert-deftest wlr-yank-undo ()
  (wlr-before-after
   "first|
second
third"
   (call-interactively 'whole-line-or-region-kill-ring-save)
   (yank)
   "first
first|
second
third"
   (let ((last-command 'yank)
         (kill-ring '("blah")))
     (yank-pop))
   "firstblah|
second
third"))

(ert-deftest wlr-kill-region ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-kill-region)
   (should (equal (current-kill 0) "first\n"))
   "sec|ond
third"
   (goto-char (point-max))
   ;; Should insert killed line before original line
   (yank)
   "second
first
third|"))

(ert-deftest wlr-consecutive-kill-region-combines-them ()
  (wlr-before-after
   "fir|st
second
third"
   (call-interactively 'whole-line-or-region-kill-region)
   (should (equal (current-kill 0) "first\n"))
   "sec|ond
third"
   (let ((last-command 'kill-region))
     (call-interactively 'whole-line-or-region-kill-region))
   (should (equal (current-kill 0) "first\nsecond\n"))
   "thi|rd"
   (yank)
   "first
second
thi|rd"
   (should (equal (point-min) (mark)))))

(ert-deftest wlr-consecutive-kill-region-combines-them-bol ()
  (wlr-before-after
   "|first
second
third"
   (call-interactively 'whole-line-or-region-kill-region)
   (should (equal (current-kill 0) "first\n"))
   "|second
third"
   (let ((last-command 'kill-region))
     (call-interactively 'whole-line-or-region-kill-region))
   (should (equal (current-kill 0) "first\nsecond\n"))
   "|third"
   (yank)
   "first
second
|third"))

(ert-deftest wlr-copy-works-without-transient-mark-mode ()
  (let (transient-mark-mode)
    (wlr-before-after
     "fir|st
second
third"
     (set-mark (point-min))
     (should (not (region-active-p)))
     (call-interactively 'whole-line-or-region-kill-ring-save)
     (should (equal (current-kill 0) "fir")))))

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
