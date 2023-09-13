;;;; development.lisp — Project Development for Webmachine

;;;; Install MacPorts (https://github.com/melusina-org/gha-install-macports)
;;;; This file is part of Install MacPorts.
;;;;
;;;; Copyright © 2022–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(require '#:org.melusina.atelier)

(defpackage #:org.melusina.gha-install-macports/development
  (:use #:common-lisp)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier))
  (:export
   #:lint))

(in-package #:org.melusina.gha-install-macports/development)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defun system-relative-pathnames (&rest pathnames)
  (mapcar #'system-relative-pathname pathnames))

(defparameter *parameter-bindings*
  '((:copyright-holder . "Michaël Le Barbier")
    (:copyright-year . "2022–2023")
    (:project-filename . "gha-install-macports")
    (:project-name . "Install MacPorts")
    (:project-description . "GitHub Action to Install MacPorts")
    (:project-long-description .
     #.(concatenate 'string
	"This GitHub Action is to install MacPorts."))
    (:homepage . "https://github.com/melusina-org/gha-install-macports")
    (:license . :mit)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     (system-relative-pathnames
      #p".github"
      #p"org.melusina.gha-install-macports.asd"
      #p"configure_macports"
      #p"development"
      #p"identify_self"
      #p"install_macports"
      #p"libexec"
      #p"subr"
      #p"testsuite"
      #p"validate_configuration"))))

;;;; End of file `development.lisp'
