;;; tox-runners-test.el --- Unit tests for supported runners

;; Copyright (C) 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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


(defconst tox--testsuite-buffer-name
  (f-join tox-testsuite-dir "resources/foo/tests/test_foo.py")
  "File name for testing.")


(ert-deftest test-tox-retrieve-root-directory ()
  :tags '(utils)
  (with-test-sandbox
   (with-current-buffer (find-file-noselect tox--testsuite-buffer-name)
     (should (string-suffix-p "tox.el/test/resources/" (tox-get-root-directory))))))



(ert-deftest test-tox-single-test-with-nose-runner ()
  :tags '(runners nose)
  (with-test-sandbox
   (let ((tox-runner 'nose))
     (with-current-buffer (find-file-noselect tox--testsuite-buffer-name)
       (save-excursion
         (re-search-forward "test_isupper")
         (with-tox current tox-default-env
                   (setq tox-default-env toxenvs)
                   (should (string= "tox  foo.tests.test_foo:TestStringMethods.test_isupper"
                                    (tox-get-command current toxenvs)))))))))


(ert-deftest test-tox-file-tests--with-nose-runner ()
  :tags '(runners nose)
  (with-test-sandbox
   (let ((tox-runner 'nose))
     (with-current-buffer (find-file-noselect tox--testsuite-buffer-name)
       (save-excursion
         (re-search-forward "TestStringMethods")
         (with-tox current tox-default-env
                   (setq tox-default-env toxenvs)
                   (should (string= "tox  foo.tests.test_foo:TestStringMethods"
                                    (tox-get-command current toxenvs)))))))))


(ert-deftest test-tox-single-test-with-pytest-runner ()
  :tags '(runners pytest)
  (with-test-sandbox
   (let ((tox-runner 'py.test))
     (with-current-buffer (find-file-noselect tox--testsuite-buffer-name)
       (save-excursion
         (re-search-forward "test_isupper")
         (with-tox current tox-default-env
                   (setq tox-default-env toxenvs)
                   (should (string= "tox  foo/tests/test_foo.py::TestStringMethods::test_isupper"
                                    (tox-get-command current toxenvs)))))))))

(ert-deftest test-tox-file-tests-with-pytest-runner ()
  :tags '(runners pytest)
  (with-test-sandbox
   (let ((tox-runner 'py.test))
     (with-current-buffer (find-file-noselect tox--testsuite-buffer-name)
       (save-excursion
         (re-search-forward "TestStringMethods")
         (with-tox current tox-default-env
                   (setq tox-default-env toxenvs)
                   (should (string= "tox  foo/tests/test_foo.py::TestStringMethods"
                                    (tox-get-command current toxenvs)))))))))



(provide 'tox-runners-test)
;;; tox-runners-test.el ends here
