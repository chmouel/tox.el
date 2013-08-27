;;; tox.el --- Launch current python test with tox.

;; Copyright (C) 2013 Chmouel Boudjnah <chmouel@chmouel.com>

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Homepage: https://github.com/chmouel/tox.el
;; Version: 20130819.1127
;; X-Original-Version: 0.1
;; Keywords: convenience tox python tests

;;; Installation:

;;; Commentary:

;; Call `tox-current-test' to launch the current test with tox.  with
;; an argument it will read the tox.ini and ask you for a value for a
;; tox environement variable.

;; Originally the ideas was coming from nosetests.el (written by me)
;; which was modified by Julien Danjou <julien@danjou.info) and
;; adapted to tox.

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

(defun tox-get-command (tox-test &optional envlist)
  "Return the command to launch tests."
    (concat
     tox-program " "
     tox-arg " "
     (if envlist (concat "-e" envlist " "))
     (subst-char-in-string
      ?/ ?.
      (file-name-sans-extension
       (substring (file-truename
                   (buffer-file-name))
                  (length (tox-get-root-directory)))))
     ":"
     tox-test))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun tox-current-test (&optional askenvs)
  "Launch tox tests, asking for a tox environement with an argument."
  (interactive "P")
  (let ((toxenvs (if askenvs
                     (completing-read
                      "Tox Environement: " (tox-read-tox-ini-envlist))
           tox-default-env))
        (default-directory (tox-get-root-directory))
        (compilation-auto-jump-to-first-error nil)
        (compilation-scroll-output nil)
        (current-function (python-info-current-defun)))
    (unless current-function
      (error "No function at point"))
    (compile (tox-get-command current-function toxenvs)))
  )

;;; End tox.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tox)

;;; tox.el ends here
