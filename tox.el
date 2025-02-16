;;; tox.el --- Run Python tests with tox -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016 Chmouel Boudjnah <chmouel@chmouel.com>
;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Homepage: https://github.com/chmouel/tox.el
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, tools, python, testing

;;; Commentary:

;; This package provides commands to run Python tests using tox:
;; - `tox-current-test': Run tox on the current test function
;; - `tox-current-class': Run tox on the current test class
;;
;; With a prefix argument, both commands will prompt for a tox environment.

;;; Code:

(require 'python)
(require 'compile)

;;; Customization

(defgroup tox nil
  "Settings for tox.el"
  :group 'python
  :prefix "tox-")

(defcustom tox-program "tox"
  "Path to tox executable."
  :type 'string
  :group 'tox)

(defcustom tox-default-env nil
  "Default tox environment to use."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Environment name"))
  :group 'tox)

(defcustom tox-runner 'nose
  "Test runner used by tox."
  :type '(choice (const :tag "nose" nose)
                 (const :tag "pytest" py.test))
  :group 'tox)

(defcustom tox-args ""
  "Additional arguments to pass to tox."
  :type 'string
  :group 'tox)

;;; Internal functions

(defun tox--find-project-root ()
  "Find the root directory containing tox.ini."
  (locate-dominating-file (or (buffer-file-name) default-directory) "tox.ini"))

(defun tox--get-envlist ()
  "Parse tox.ini and return list of environments."
  (let ((tox-ini (expand-file-name "tox.ini" (tox--find-project-root))))
    (when (file-exists-p tox-ini)
      (with-temp-buffer
        (insert-file-contents tox-ini)
        (when (re-search-forward "^envlist\\s-*=\\s-*\\([^[:space:]]+\\)" nil t)
          (split-string (match-string 1) "," t "\\s-*"))))))

(defun tox--get-relative-path ()
  "Get current file path relative to project root."
  (file-relative-name (buffer-file-name) (tox--find-project-root)))

(defun tox--format-test-path (test-name)
  "Format TEST-NAME for the current test runner."
  (pcase tox-runner
    ('nose
     (concat (subst-char-in-string ?/ ?.
              (file-name-sans-extension (tox--get-relative-path)))
             ":" test-name))
    ('py.test
     (concat (tox--get-relative-path)
             "::"
             (replace-regexp-in-string (regexp-quote ".") "::" test-name)))
    (_ (error "Unknown test runner: %s" tox-runner))))

(defun tox--build-command (test-path &optional env)
  "Build tox command for TEST-PATH and optional ENV."
  (concat tox-program " "
          (when (not (string-empty-p tox-args))
            (concat tox-args " "))
          (when env
            (concat "-e" env " "))
          (tox--format-test-path test-path)))

;;; Interactive commands

;;;###autoload
(defun tox-current-test (&optional ask-env)
  "Run tox on the current test.
With ASK-ENV or no default env set, prompt for environment."
  (interactive "P")
  (let* ((test-name (python-info-current-defun))
         (root-dir (tox--find-project-root))
         (env (if (or ask-env (null tox-default-env))
                 (completing-read "Tox Environment: " (tox--get-envlist))
               tox-default-env)))
    
    (unless test-name
      (user-error "No test function at point"))
    (unless root-dir
      (user-error "No tox.ini found"))

    (setq tox-default-env env)
    (let ((default-directory root-dir)
          (compilation-auto-jump-to-first-error nil)
          (compilation-scroll-output nil))
      (compile (tox--build-command test-name env)))))

;;;###autoload
(defun tox-current-class (&optional ask-env)
  "Run tox on the current test class.
With ASK-ENV or no default env set, prompt for environment."
  (interactive "P")
  (let* ((full-name (python-info-current-defun))
         (root-dir (tox--find-project-root))
         (env (if (or ask-env (null tox-default-env))
                 (completing-read "Tox Environment: " (tox--get-envlist))
               tox-default-env)))
    
    (unless full-name
      (user-error "No class at point"))
    (unless root-dir
      (user-error "No tox.ini found"))

    (let* ((class-name (car (split-string full-name "\\.")))
           (default-directory root-dir)
           (compilation-auto-jump-to-first-error nil)
           (compilation-scroll-output nil))
      (setq tox-default-env env)
      (compile (tox--build-command class-name env)))))

(provide 'tox)
;;; tox.el ends here
