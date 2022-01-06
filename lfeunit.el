;;; lfeunit.el --- `LFE test' runner -*- lexical-binding: t -*-

;; Copyright (C) 2020 Manfred Bergmann.

;; Author: Manfred Bergmann <manfred.bergmann@me.com>
;; URL: http://github.com/mdbergmann/lfeunit
;; Version: 0.1
;; Keywords: processes lfe ltest rebar3 test
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides commands to run test cases in a LFE project.

;;; Code:

(require 'ansi-color)

(make-variable-buffer-local
 (defvar lfeunit-mode))

(defvar *lfeunit-output-buf-name* "*LFEUnit output*")

(defun lfeunit-execute-test ()
  "Call Lfe test."
  (let* ((test-cmd-args (list "rebar3" "lfe" "ltest" "-t" "unit"))
         (call-args
          (append (list (car test-cmd-args) nil *lfeunit-output-buf-name* t)
                  (cdr test-cmd-args))))
    (message "calling: %s" call-args)
    (let* ((default-directory (locate-dominating-file default-directory "rebar.config"))
           (call-result (apply 'call-process call-args)))
      (message "cwd: %s" default-directory)
      (message "test call result: %s" call-result)
      call-result)))

(defun lfeunit-handle-successful-test-result ()
  "Do some stuff when the tests ran OK."
  (message "%s" (propertize "Tests OK" 'face '(:foreground "green"))))

(defun lfeunit-handle-unsuccessful-test-result ()
  "Do some stuff when the tests ran NOK."
  (message "%s" (propertize "Tests failed!" 'face '(:foreground "red"))))

(defun lfeunit-after-save-action ()
  "Execute the test."
  (message "lfeunit: after save action from in: %s" major-mode)

  ;; delete output buffer contents
  (when (get-buffer *lfeunit-output-buf-name*)
    (with-current-buffer *lfeunit-output-buf-name*
      (erase-buffer)))
  
  (let ((test-result (cond
                      ((string-equal "lfe-mode" major-mode)
                       (lfeunit-execute-test))
                      (t (progn
                           (message "Unknown mode!")
                           nil)))))
    (unless (eq test-result nil)
      (if (= test-result 0)
          (lfeunit-handle-successful-test-result)
        (lfeunit-handle-unsuccessful-test-result))
      (with-current-buffer *lfeunit-output-buf-name*
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun lfeunit-execute ()
  "Save buffers and execute command to run the tests."
  (interactive)
  (message "Saving own buffer...")
  (save-buffer)
  (message "Saving other buffers...")
  (save-some-buffers)
  (lfeunit-after-save-action))

(define-minor-mode lfeunit-mode
  "Lfe - test runner. Runs a command that runs tests."
  :lighter " LFEUnit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'lfeunit-execute)
            map))

(provide 'lfeunit)
;;; lfeunit.el ends here
