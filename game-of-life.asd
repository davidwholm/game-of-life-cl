(defsystem "game-of-life"
  :version "0.1.0"
  :author "davidwholm"
  :license "MIT"
  :depends-on (:cl-raylib)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Simple GOL implementation")
