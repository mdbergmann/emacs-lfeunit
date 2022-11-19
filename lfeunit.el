;;; lfeunit.el --- `LFE test' runner -*- lexical-binding: t -*-

;; Copyright (C) 2020-endless Manfred Bergmann.

;; Author: Manfred Bergmann <manfred.bergmann@me.com>
;; URL: http://github.com/mdbergmann/emacs-lfeunit
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
(require 'cl-lib)

(make-variable-buffer-local
 (defvar lfeunit-mode))

(defvar lfeunit-test-failure-hook nil)
(defvar lfeunit-test-success-hook nil)

(defvar-local *last-test* nil)
(defvar-local *test-process* nil)

(defvar lfeunit-test-cmd-base (split-string "rebar3 as test eunit")
  "The test command to execute.")

(defvar *lfeunit-output-buf-name* "*LFEUnit output*")

(defun lfeunit--find-module (buffer-text)
  "Parse the module for the test run.
This is usually the full designated module as per `defmodule'.
BUFFER-TEXT is a string where the matching should take place."
  (let ((module-string (progn
                         (string-match "(defmodule[ ]+\\([[:alpha:]|-]+\\).*$"
                                       buffer-text)
                         (match-string 1 buffer-text))))
    (message "Module to test: %s" module-string)
    module-string))

(defun lfeunit--find-test-function (buffer-text curr-position)
  "Parse the function for the test run.
This is test function definition as per `deftest'.
BUFFER-TEXT is a string where the matching should take place.
CURR-POSITION is the current curser position in buffer."
  (message "Finding test function in module.")
  (with-temp-buffer
    (insert buffer-text)
    (goto-char curr-position)
    (let ((found-point (search-backward "(deftest" nil t)))
      (message "point: %s" found-point)
      (if found-point
          (let ((matches (string-match "(deftest[ ]+\\(.*\\)\n.*"
                                       buffer-text
                                       (- found-point 1))))
            (message "matches: %s" matches)
            (let* ((match (match-string 1 buffer-text))
                   (test-function (format "%s_test"
                                          (string-replace "-" "_" match))))
              (message "Function to test: %s" test-function)
              test-function))))))

(defun lfeunit--project-root-dir ()
  "Return the project root directory."
  (locate-dominating-file default-directory "rebar.config"))

(defun lfeunit--process-filter (proc string)
  "Process filter function. Takes PROC as process.
And STRING as the process output.
The output as STRING is enriched with text attributes from ansi escape commands."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun lfeunit--process-sentinel (proc signal)
  "Process sentinel of the command we've run for the test.
PROC is the process. SIGNAL the signal from the process."
  (ignore signal)
  (let ((process-rc (process-exit-status proc)))
    (with-current-buffer (process-buffer proc)
      (ansi-color-apply-on-region (point-min) (point-max)))
    (if (= process-rc 0)
        (lfeunit--handle-successful-test-result)
      (lfeunit--handle-unsuccessful-test-result)))
  (when (not (process-live-p proc))
    (setq *test-process* nil)))

(defun lfeunit--execute-test-in-context (test-args)
  "Call specific test. TEST-ARGS specifies a test to run."
  (message "Run with test args: %s" test-args)
  (let ((test-cmd-args (append
                        lfeunit-test-cmd-base
                        test-args)))
    (message "Calling: %s" test-cmd-args)
    (let ((default-directory (lfeunit--project-root-dir)))
      (message "cwd: %s" default-directory)
      (setq *test-process*
            (make-process :name "lfeunit"
                          :buffer *lfeunit-output-buf-name*
                          :command test-cmd-args
                          :filter 'lfeunit--process-filter
                          :sentinel 'lfeunit--process-sentinel))
      (message "Running: %s" test-args))))

(defun lfeunit--compute-test-args (test-spec single buffer-text curr-point)
  "Calculates test-args as used in execute-in-context.
TEST-SPEC is a given, previously executed test.
When this is not null it'll be used.
Otherwise we calculate a new test-spec, from module.
SINGLE defines if single test function should run (t/nil).
BUFFER-TEXT contains the buffer text as string without properties.
CURR-POINT is the current cursor position.
Only relevant if SINGLE is specified."
  (if (not (null test-spec))
      test-spec
    (let* ((test-module (lfeunit--find-module buffer-text))
           (test-args
            (if single
                (let ((test-function (lfeunit--find-test-function
                                      buffer-text
                                      curr-point)))
                  (list "-t" (format "%s:%s" test-module test-function)))
              (list "-m" test-module))))
      (message "test args: %s" test-args)
      test-args)))

(cl-defun lfeunit--get-buffer-text (&optional (beg 1) (end (point-max)))
  "Retrieve the buffer text. Specify BEG and END for specific range."
  (buffer-substring-no-properties beg end))

(defun lfeunit--handle-successful-test-result ()
  "Do some stuff when the test ran OK."
  (message "LFEUNIT: running success hook!")
  (run-hooks 'lfeunit-test-success-hook)
  (message "%s" (propertize "Tests OK" 'face '(:foreground "green"))))

(defun lfeunit--handle-unsuccessful-test-result ()
  "Do some stuff when the test ran NOK."
  (message "LFEUNIT: running failure hook!")
  (run-hooks 'lfeunit-test-failure-hook)
  (message "%s" (propertize "Tests failed!" 'face '(:foreground "red"))))

(cl-defun lfeunit--run-test (&optional (test-spec nil) (single nil))
  "Execute the test.
Specify optional TEST-SPEC if a specific test should be run.
Specify optional SINGLE (T)) to try to run only a single test case."
  (message "lfeunit: run-test")

  (unless (string-equal "lfe-mode" major-mode)
    (error "Need 'lfe-mode' to run!"))
  
  (get-buffer-create *lfeunit-output-buf-name*)

  (with-current-buffer *lfeunit-output-buf-name*
    (erase-buffer))
  
  (let ((test-args (lfeunit--compute-test-args
                    test-spec
                    single
                    (lfeunit--get-buffer-text)
                    (point))))
    (lfeunit--execute-test-in-context test-args)
    (setq-local *last-test* test-args)))

(defun lfeunit-custom-cmd ()
  "Being prompted for a custom command to execute as test command."
  (interactive)
  (setq lfeunit-test-cmd-base (split-string
                               (read-string "[lfeunit] Enter base test cmd: "))))

(defun lfeunit-run-all ()
  "Save buffers and execute all test cases in the context."
  (interactive)
  (when (lfeunit--run-preps)
    (lfeunit--run-test)))

(defun lfeunit-run-single ()
  "Save buffers and execute single test cases in the context."
  (interactive)
  (when (lfeunit--run-preps)
    (lfeunit--run-test nil t)))

(defun lfeunit-run-last ()
  "Save buffers and execute command to run the test."
  (interactive)
  (when (lfeunit--run-preps)
    (lfeunit--run-test *last-test*)))

(defun lfeunit--run-preps ()
  "Check if we can run."
  (if (or (null *test-process*) (not (process-live-p *test-process*)))
      (progn
        (save-buffer)
        (save-some-buffers)
        t)
    (progn
      (message "Test still running. Try again when finished!")
      nil)))

(define-minor-mode lfeunit-mode
  "Bloop unit - test runner. Runs Bloop to execute tests."
  :lighter " LFEU"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'lfeunit-run-all)
            (define-key map (kbd "C-c C-s") 'lfeunit-run-single)
            (define-key map (kbd "C-c C-r") 'lfeunit-run-last)
            (define-key map (kbd "C-c C-p") 'lfeunit-custom-cmd)
            map))

(provide 'lfeunit)

;; ---------------------------------------------
;; tests ;; more tests
;; ---------------------------------------------

(defvar lfeunit--run-tests nil)

(eval-when-compile
  (setq lfeunit--run-tests t))

(defun lfeunit--test--find-test-module ()
  "Test finding the class context."
  (message "lfeunit--test--find-test-module...")
  (let ((buffer-text "(defmodule foo-bar-tests
"))
    (cl-assert (string= "foo-bar-tests"
                        (lfeunit--find-module buffer-text)))))

(defun lfeunit--test--find-test-function ()
  "Test finding the test-case context - deftest."
  (let ((buffer-text "some stuff
  (deftest foo-bar-test
    (is 'true))
"))
    (cl-assert (string= "foo_bar_test_test"
                        (lfeunit--find-test-function buffer-text 40)))))

(defun lfeunit--test--compute-test-args ()
  "Test computing test args."
  (message "lfeunit--test--compute-test-args...")
  (let ((buffer-text ""))
    ;; return given test spec
    (cl-assert (string= "my-given-test-spec"
                        (lfeunit--compute-test-args "my-given-test-spec" nil buffer-text 0))))
  (let ((buffer-text "(defmodule foo-bar-tests\n"))
    ;; return full test module
    (cl-assert (cl-equalp (list "-m" "foo-bar-tests")
                          (lfeunit--compute-test-args nil nil buffer-text 0))))
  (let ((buffer-text "(defmodule foo-bar-tests)
(deftest foo
  (is 'true))
)"))
    ;; return modue:function
    (cl-assert (cl-equalp (list "-t" "foo-bar-tests:foo_test")
                          (lfeunit--compute-test-args nil t buffer-text 40))))
  )

(when lfeunit--run-tests
  (lfeunit--test--find-test-module)
  (lfeunit--test--find-test-function)
  (lfeunit--test--compute-test-args)
  )

;;; lfeunit.el ends here
