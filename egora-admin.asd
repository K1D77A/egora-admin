;;;; admin.asd

(asdf:defsystem #:egora-admin
  :description "Describe admin her"
  :author "K1D77A"
  :license  "APACHE 2.0"
  :version "0.0.1"
  :depends-on (#:jonathan
               #:alexandria
               #:str
               #:dexador
               #:lass
               #:ironclad
               #:hunchentoot
               #:spinneret)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "classes")
               (:file "tests")
               (:file "egora-admin")
               (:file "user-api")
               (:file "admin-api")
               (:file "general-commands")
               (:file "site/package")
               (:file "site/site")))
