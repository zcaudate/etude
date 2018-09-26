;; Modern libraries
(use-package s)     ;; string
(use-package dash)  ;; list
(use-package ht)    ;; maps
(use-package f)     ;; file
(use-package hydra) ;; menus

;; Macro Definitions
(defun let:fn (bindings &rest body)
  (let ((bargs (seq-reverse (seq-partition bindings 2))))
    (seq-reduce (lambda (body barg)
                  (list '-let barg body))
                bargs
                (cons 'progn body))))

(defmacro let: (bindings &rest body)
  (declare (indent 1))
  (apply 'let:fn bindings body))

(defmacro comment: (&rest body)
  (declare (indent 1)))

(defmacro fn: (bindings &rest body)
  (declare (indent 1)))

(defmacro loop: (bindings &rest body)
  (declare (indent 1)))
  
(defmacro ns: (name &rest body)
  (declare (indent 1)))

;;
;; commands and menus
;;

(setq on/*lock* nil)
(setq on/*commands* (ht-create))
(setq on/*meta-config* "M-0")

(defun on/put-command (key fn)
  (if (and on/*lock*
           (ht-get on/*commands* key))
      (error (s-concat "key " (symbol-name key) " already exists"))
    (ht-set on/*commands* key fn)))

(defun on/get-command (key)
  (gethash key on/*commands*))
  (defun on/bind:fn (declaration &rest specs)
    (let: [bind-map (if (seq-empty-p declaration)
                        nil
                      (seq-elt declaration 0))
           body (seq-mapcat (lambda (spec)
                              (let: [(key bindings fn) spec]
                                (if fn
                                    (progn (on/put-command key (cadr fn))
                                           (seq-map (lambda (binding)
                                                      `(progn ,(if bind-map
                                                                   `(bind-key ,binding ,fn ,bind-map)
                                                                 `(bind-key* ,binding ,fn))
                                                              (vector ,key ,binding ,fn)))
                                                    bindings)))))
                            (seq-partition specs 3))]
      (cons 'list body)))

(defmacro on/bind: (declaration &rest specs)
  (declare (indent 1))
  (apply 'on/bind:fn declaration specs))

(defun on/menu:fn (declaration layout &rest specs)
  (let: [[key bindings] declaration
         menu-name   (intern (s-concat "on/menu-fn" (symbol-name key)))
         body-name   (intern (s-concat (symbol-name menu-name) "/body"))]
    `(list (defhydra ,menu-name ()
             ,layout
             ,@(seq-map (lambda (spec)
                          (let: [(binding key text . more) spec
                                 fn (on/get-command key)]
                            (apply 'list binding fn text more)))
                        specs))
           (on/bind: [] ,key ,bindings (quote ,body-name)))))

(defmacro on/menu: (declaration layout &rest specs)
  (declare (indent 1))
  (apply 'on/menu:fn declaration layout specs))

(setq on/*mode-bindings*  (ht-create))
(setq on/*mode-functions* (ht-create))
(setq on/*mode-lookup*    (ht-create))

(defun on/mode-key ()
  (ht-get on/*mode-lookup* major-mode))

(defun on/mode-dispatch (fn-key &rest args)
  (let: [mode-key (ht-get on/*mode-lookup* major-mode)
         fn-table (if mode-key
                      (ht-get on/*mode-functions* mode-key))
         fn       (if fn-table
                      (ht-get fn-table fn-key))]
    (if fn
        (apply 'funcall fn args)
      (error (s-concat "Function unavailable ("
                       (symbol-name mode-key)
                       " "
                       (symbol-name fn-key) ")")))))

(defun on/mode-init:create-mode-fn (fn-key bindings params)
  (let: [fn-name (intern (s-concat "on/mode-fn"
                                   (symbol-name fn-key)))
         args    (seq-map 'intern params)]
    (ht-set on/*mode-bindings* fn-key bindings)
    `(progn (defun ,fn-name (,@args)
              (interactive ,@params)
              (on/mode-dispatch ,fn-key ,@args))
            (on/bind: nil ,fn-key ,bindings (quote ,fn-name)))))

(defmacro on/mode-init: (&rest specs)
  (let: [body (seq-map (lambda (args)
                         (apply 'on/mode-init:create-mode-fn args))
                       (seq-partition specs 3))]
    (cons 'progn body)))

(defun on/mode:create-config-fn (mode-key mode-name mode-config)
  (let: [fn-name   (intern (s-concat "on/mode-config" (symbol-name mode-key)))
         mode-map  (intern (s-concat (symbol-name mode-name) "-map"))]
    `(progn
       (defun ,fn-name ()
         (interactive)
         (on/jump-to-config ,mode-config))
       (bind-key on/*meta-config* (quote ,fn-name) ,mode-map))))

(defun on/mode:fn (declaration mode-config &rest specs)
  (let: [[mode-key mode-name] declaration
         mode-table (ht-create)
         _    (ht-set on/*mode-functions* mode-key mode-table)
         _    (ht-set on/*mode-lookup* mode-name mode-key)
         conf-body (on/mode:create-config-fn mode-key mode-name mode-config)
         body      (seq-map (lambda (spec)
                              (let: [(fn-key fn) spec
                                     mode-fn-key (intern (s-concat (symbol-name fn-key) (symbol-name mode-key)))]
                                (ht-set mode-table fn-key (cadr fn))))
                            (seq-partition specs 2))]
    conf-body))

(defmacro on/mode: (declaration config &rest specs)
  (declare (indent 1))
  (apply 'on/mode:fn declaration config specs))

;; str/
;; seq/
;; seq/

(provide 'etude-lang)
