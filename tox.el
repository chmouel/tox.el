;;; tox.el --- Launch current python test with tox

;; Copyright (C) 2013 Chmouel Boudjnah <chmouel@chmouel.com>

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Homepage: https://github.com/chmouel/tox.el
;; Version: 20130831.1352
;; X-Original-Version: 0.1
;; Keywords: convenience, tox, python, tests

;;; Installation:

;;; Commentary:

;; Call `tox-current-test', `tox-current-class' to launch the current
;; test or class with tox.  with an argument it will read the tox.ini
;; and ask you for a value for a tox environement variable.

;; To use this code, bind the functions `tox-current-test',
;; `tox-current-class', `tox-current-module' and `tox-current-project'
;;  to convenient keys with something like:

;;
;; (define-key python-mode-map (kbd "C-c t") 'tox-current-test)
;; (define-key python-mode-map (kbd "C-c c") 'tox-current-class)
;; (define-key python-mode-map (kbd "C-c m") 'tox-current-module)
;; (define-key python-mode-map (kbd "C-c p") 'tox-current-project)
;;

;; Originally the ideas was coming from nosetests.el (written by me)
;; which was modified by Julien Danjou <julien@danjou.info) and
;; adapted to tox.

;;; TODO:

;; - Refactorize tox-current-test and tox-current-class in one.
;; - Don't read multiple times tox.ini for same project.

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; Default setting lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tox-program "tox"
  "Tox binary path.")

(defvar tox-arg ""
  "Argument to pass to tox.")

(defvar tox-default-env nil
  "Default argument for Tox")

;;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tox-read-tox-ini-envlist()
  "Read the tox.ini file and grab the environement list."
  (let ((tox-ini-file
         (concat (locate-dominating-file
                  (buffer-file-name) "tox.ini") "tox.ini"))
        (envlist))
    (with-temp-buffer
      (buffer-disable-undo)
      (cond ((get-file-buffer tox-ini-file)
             (insert (with-current-buffer (get-file-buffer tox-ini-file)
                       (buffer-substring (point-min) (point-max)))))
            ((not (file-exists-p tox-ini-file)))
            (t (insert-file-contents tox-ini-file)))
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n) (newline))
      (goto-char (point-min))
      (while (re-search-forward "^envlist\s*=\s*\\([^\t\n ]+\\)" nil t)
        (setq envlist
          (split-string (buffer-substring-no-properties
                         (match-beginning 1)(match-end 1)) ","))))
    envlist))

(defun tox-get-root-directory()
  "Return the root directory to run tests."
  (file-truename (or (locate-dominating-file
                      (buffer-file-name) "tox.ini")
                     "./")))

(defun tox-extract-path ()
  "Extract python module from pathname."
  (subst-char-in-string
      ?/ ?.
      (file-name-sans-extension
       (substring (file-truename
                   (buffer-file-name))
                  (length (tox-get-root-directory))))))

(defun tox-get-command (tox-test &optional envlist)
  "Return the command to launch tests."
    (concat
     tox-program " "
     tox-arg " "
     (if envlist (concat "-e" envlist " "))
     tox-test))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro with-tox (current &optional askenvs &rest body)
  "Macro which initialize environments variables to launch unit tests."
    `(let ((toxenvs (if ,askenvs
			(completing-read
			 "Tox Environement: " (tox-read-tox-ini-envlist))
		      tox-default-env))
	   (default-directory (tox-get-root-directory))
	   (compilation-auto-jump-to-first-error nil)
	   (compilation-scroll-output nil)
	   (,current (python-info-current-defun)))
       ,@body))


;;;###autoload
(defun tox-current-test (&optional askenvs)
  "Launch tox on current test.
A prefix arg will ask for a env to use which is by default what
specified in `tox-default-env'."
  (interactive "P")
  (with-tox current askenvs
     (unless current
       (error "No function at point"))
     (compile (tox-get-command (concat (tox-extract-path) ":" current)
			       toxenvs))))

;;;###autoload
(defun tox-current-class (&optional askenvs)
  "Launch tox on current class.
A prefix arg will ask for a env to use which is by default what
specified in `tox-default-env'."
  (interactive "P")
  (with-tox current askenvs
     (if current
	 (let ((current-class (car (split-string current "\\."))))
	   (compile (tox-get-command (concat (tox-extract-path) ":" current-class)
				     toxenvs)))
       (error "No class at point"))))


;;;###autoload
(defun tox-current-module (&optional askenvs)
  "Launch tox on current module.
A prefix arg will ask for a env to use which is by default what
specified in `tox-default-env'."
  (interactive "P")
  (with-tox current askenvs
     (if current
	 (compile (tox-get-command (tox-extract-path) toxenvs)))))


;;;###autoload
(defun tox-current-project (&optional askenvs)
  "Launch tox on current project.
A prefix arg will ask for a env to use which is by default what
specified in `tox-default-env'."
  (interactive "P")
  (with-tox current askenvs
     (if current
	 (compile (tox-get-command "" toxenvs)))))

;;; End tox.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tox)

;;; tox.el ends here
