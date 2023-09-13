;;;; org.melusina.gha-install-macports.asd — System definition Install Macports

;;;; Install MacPorts (https://github.com/melusina-org/gha-install-macports)
;;;; This file is part of Install MacPorts.
;;;;
;;;; Copyright © 2022–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.gha-install-macports/development
  :description "Development tools for the GitHub Action Install MacPorts"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

;;;; End of file `org.melusina.gha-install-macports.asd'
