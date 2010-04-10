(asdf:defsystem #:l-system
  :depends-on (#:lispbuilder-sdl)
  :components
  ((:module "src"
      :serial t
      :components
        ((:file "dragon-curve")
        (:file "frontend")))))