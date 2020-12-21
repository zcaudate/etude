;; ETUDE LANG
;;
;; This package defines typical code convensions that
;; will be used. This follows governing clojure principles
;; in terms of readability and code maintainance.
;;
(require 'etude-boot)

;; Modern libraries
(use-package s :ensure t)     ;; string
(use-package dash :ensure t)  ;; list
(use-package ht :ensure t)    ;; maps
(use-package f :ensure t)     ;; file
(use-package hydra :ensure t) ;; menus
(use-package pretty-hydra :ensure t) ;; pretty menus

;; Macro Definitions
(defun e/let:fn (bindings &rest body)
  (let ((bargs (seq-reverse (seq-partition bindings 2))))
    (seq-reduce (lambda (body barg)
                  (list '-let barg body))
                bargs
                (cons 'progn body))))

(defmacro e/let (bindings &rest body)
  (declare (indent 1))
  (apply 'e/let:fn bindings body))

(defmacro e/comment (&rest body))

(defmacro comment (&rest body))

(setq e/*lock* nil)
(setq e/*commands* (ht-create))
(setq e/*meta-config* "M-0")

(defun e/put-command (key fn)
  (if (and e/*lock*
           (ht-get e/*commands* key))
      (error (s-concat "key " (symbol-name key) " already exists"))
    (ht-set e/*commands* key fn)))

(defun e/get-command (key)
  (gethash key e/*commands*))

;;
;; e/bind
;;

(defun e/bind:fn (declaration &rest specs)
  (e/let [bind-map (if (seq-empty-p declaration)
                       nil
                     (seq-elt declaration 0))
          body (seq-mapcat (lambda (spec)
                             (e/let [(key bindings fn) spec]
                               (if fn
                                   (progn (e/put-command key (cadr fn))
                                          (seq-map (lambda (binding)
                                                     `(progn ,(if bind-map
                                                                  (if (eq bind-map '*)
                                                                      `(bind-key*, binding ,fn)
                                                                    `(bind-key ,binding ,fn ,bind-map))
                                                                `(bind-key, binding ,fn))
                                                             (vector ,key ,binding ,fn)))
                                                   bindings)))))
                           (seq-partition specs 3))]
    (cons 'list body)))

(defmacro e/bind (declaration &rest specs)
  (declare (indent 1))
  (apply 'e/bind:fn declaration specs))

;;
;; e/menu
;;

(defun e/menu:fn (declaration layout &rest specs)
  (e/let [[key bindings] declaration
         menu-name   (intern (s-concat "e/menu-fn" (symbol-name key)))
         body-name   (intern (s-concat (symbol-name menu-name) "/body"))]
    `(list (defhydra ,menu-name ()
             ,layout
             ,@(seq-map (lambda (spec)
                          (e/let [(binding key text . more) spec
                                 fn (e/get-command key)]
                            (apply 'list binding fn text more)))
                        specs))
           (e/bind [] ,key ,bindings (quote ,body-name)))))

(defmacro e/menu (declaration layout &rest specs)
  (declare (indent 1))
  (apply 'e/menu:fn declaration layout specs))

;;
;; setup for e/mode
;;

(setq e/*mode-bindings*  (ht-create))
(setq e/*mode-functions* (ht-create))
(setq e/*mode-lookup*    (ht-create))

(defun e/mode-key ()
  (ht-get e/*mode-lookup* major-mode))


(defun e/mode-dispatch (fn-key &rest args)
  (e/let [mode-key (ht-get e/*mode-lookup* major-mode)
         fn-table (if mode-key
                      (ht-get e/*mode-functions* mode-key))
         fn       (if fn-table
                      (ht-get fn-table fn-key))]
    (if fn
        (apply 'funcall fn args)
      (error (s-concat "Function unavailable ("
                       (symbol-name mode-key)
                       " "
                       (symbol-name fn-key) ")")))))

;;
;; e/mode-init
;;

(defun e/mode-init:create-mode-fn (fn-key bindings params)
  (e/let [fn-name (intern (s-concat "e/mode-fn"
                                   (symbol-name fn-key)))
         args    (seq-map 'intern params)]
    (ht-set e/*mode-bindings* fn-key bindings)
    `(progn (defun ,fn-name (,@args)
              (interactive ,@params)
              (e/mode-dispatch ,fn-key ,@args))
            (e/bind nil ,fn-key ,bindings (quote ,fn-name)))))

(defun e/mode-init:form (declaration &rest specs)
  (e/let [body (seq-map (lambda (args)
                         (apply 'e/mode-init:create-mode-fn args))
                       (seq-partition specs 3))]
    (cons 'progn body)))

(defmacro e/mode-init (declaration &rest specs)
  (declare (indent 1))
  (apply 'e/mode-init:form declaration specs))

;;
;; e/mode
;;

(defun e/mode:create-config-fn (mode-key mode-name mode-config)
  (e/let [fn-name   (intern (s-concat "e/mode-config" (symbol-name mode-key)))
         mode-map  (intern (s-concat (symbol-name mode-name) "-map"))]
    `(progn
       (defun ,fn-name ()
         (interactive)
         (e/jump-to-config ,mode-config))
       (bind-key e/*meta-config* (quote ,fn-name) ,mode-map))))

(defun e/mode:form (declaration &rest specs)
  (e/let [[mode-key mode-name &rest more] declaration
         mode-file-name (if (not (seq-empty-p more)) (seq-elt more 0))
         mode-table (ht-create)
         _    (ht-set e/*mode-functions* mode-key mode-table)
         _    (ht-set e/*mode-lookup* mode-name mode-key)
         conf-body (e/mode:create-config-fn mode-key mode-name (or mode-file-name load-file-name))
         body      (seq-map (lambda (spec)
                              (e/let [(fn-key fn) spec
                                     mode-fn-key (intern (s-concat (symbol-name fn-key) (symbol-name mode-key)))]
                                (ht-set mode-table fn-key (cadr fn))))
                            (seq-partition specs 2))]
    conf-body))

(defmacro e/mode (declaration config &rest specs)
  (declare (indent 1))
  (apply 'e/mode:form declaration config specs))

(provide 'etude-lang)
