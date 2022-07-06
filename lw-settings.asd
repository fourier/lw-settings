;;;; lw-settings.asd

(asdf:defsystem #:lw-settings
  :description "Wrapper around LispWorks user-preference API"
  :author "Alexey Veretennikov <alexey.veretennikov@protonmail.com>"
  :license "MIT"
  :serial t
  :components (#+lispworks(:file "lw-settings")))