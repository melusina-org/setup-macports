;;;; setup.lisp — Setup for Atelier

;;;; Install MacPorts (https://github.com/melusina-org/gha-install-macports)
;;;; This file is part of Install MacPorts.
;;;;
;;;; Copyright © 2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

;;;
;;; Atelier
;;;

(ql:quickload "org.melusina.atelier" :silent t)

(org.melusina.atelier:initialise)

(setf org.melusina.atelier:*parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2022")
	(:project-filename . "gha-install-macports")
        (:project-name . "Install MacPorts")
	(:project-description . "GitHub Action to Install MacPorts")
        (:project-long-description .
	 #.(concatenate 'string
	    "This GitHub Action is to install MacPorts."))
        (:homepage . "https://github.com/melusina-org/gha-install-macports")
        (:license . :mit)))

;;;; End of file `setup.lisp'
