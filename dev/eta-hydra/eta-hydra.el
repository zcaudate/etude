;; ETUDE LANG
;;
;; This package defines typical code convensions that
;; will be used. This follows governing clojure principles
;; in terms of readability and code maintainance.
;;

(require 'dash)
(require 'eta)
(require 'hydra)

;;
;; e/menu
;;

(defun eta-hydra-fn (declaration layout &rest specs)
  (-let* ((key bindings) declaration)
          (menu-name   (intern (concat "eta-hydra-fn" (symbol-name key))))
          (body-name   (intern (concat (symbol-name menu-name) "/body"))))
    `(list (defhydra ,menu-name ()
             ,layout
             ,@(seq-map (lambda (spec)
                          (-let* (((binding key text . more) spec)
                                  (fn (eta-get-command key)))
                            (apply 'list binding fn text more))
                        specs))
           (eta-bind [] ,key ,bindings (quote ,body-name)))))

(defmacro eta-hydra (declaration layout &rest specs)
  (declare (indent 1))
  (apply 'eta-hydra-fn declaration layout specs))

(provide 'eta-hydra)
