;; ETUDE LANG
;;
;; This package defines typical code convensions that
;; will be used. This follows governing clojure principles
;; in terms of readability and code maintainance.
;;

;; Modern libraries
(use-package s)     ;; string
(use-package dash)  ;; list
(use-package ht)    ;; maps
(use-package f)     ;; file
(use-package hydra) ;; menus

;; Fix vector intentation

(defun on/lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (or (not (eq ?\( (char-before)))
            (and (elt state 2)
                 (not (and (looking-at "\\sw\\|\\s_")
                           (not (eq ?: (char-after)))))))
        ;; indent as data
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      ;; indent as function
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state)))))))

(setq-default lisp-indent-function 'on/lisp-indent-function)

;; Macro Definitions
(defun let::fn (bindings &rest body)
  (let ((bargs (seq-reverse (seq-partition bindings 2))))
    (seq-reduce (lambda (body barg)
                  (list '-let barg body))
                bargs
                (cons 'progn body))))

(defmacro let: (bindings &rest body)
  (declare (indent 1))
  (apply 'let::fn bindings body))

(defmacro comment: (&rest body)
  (declare (indent 1)))

(defmacro loop: (bindings &rest body)
  (declare (indent 1)))

(defun ns::require (forms)
  (let: [(_ . packages) forms]
    (seq-map (lambda (p)
               `(require (quote ,p)))
             packages)))

(defmacro ns: (name &rest body)
  (declare (indent 1))
  (let: [head `(provide (quote ,name))]
    `(progn ,head
            ,@(seq-mapcat 'ns::require body))))

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

;;
;; on/bind:
;;

(defun on/bind::fn (declaration &rest specs)
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
  (apply 'on/bind::fn declaration specs))

;;
;; on/menu:
;;

(defun on/menu::fn (declaration layout &rest specs)
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
  (apply 'on/menu::fn declaration layout specs))

;;
;; setup for on/mode:
;;

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

;;
;; on/mode-init:
;;

(defun on/mode-init::create-mode-fn (fn-key bindings params)
  (let: [fn-name (intern (s-concat "on/mode-fn"
                                   (symbol-name fn-key)))
         args    (seq-map 'intern params)]
    (ht-set on/*mode-bindings* fn-key bindings)
    `(progn (defun ,fn-name (,@args)
              (interactive ,@params)
              (on/mode-dispatch ,fn-key ,@args))
            (on/bind: nil ,fn-key ,bindings (quote ,fn-name)))))

(defun on/mode-init::form (declaration &rest specs)
  (let: [body (seq-map (lambda (args)
                         (apply 'on/mode-init::create-mode-fn args))
                       (seq-partition specs 3))]
    (cons 'progn body)))

(defmacro on/mode-init: (declaration &rest specs)
  (declare (indent 1))
  (apply 'on/mode-init::form declaration specs))

;;
;; on/mode:
;;

(defun on/mode::create-config-fn (mode-key mode-name mode-config)
  (let: [fn-name   (intern (s-concat "on/mode-config" (symbol-name mode-key)))
         mode-map  (intern (s-concat (symbol-name mode-name) "-map"))]
    `(progn
       (defun ,fn-name ()
         (interactive)
         (on/jump-to-config ,mode-config))
       (bind-key on/*meta-config* (quote ,fn-name) ,mode-map))))

(defun on/mode::form (declaration mode-config &rest specs)
  (let: [[mode-key mode-name] declaration
         mode-table (ht-create)
         _    (ht-set on/*mode-functions* mode-key mode-table)
         _    (ht-set on/*mode-lookup* mode-name mode-key)
         conf-body (on/mode::create-config-fn mode-key mode-name mode-config)
         body      (seq-map (lambda (spec)
                              (let: [(fn-key fn) spec
                                     mode-fn-key (intern (s-concat (symbol-name fn-key) (symbol-name mode-key)))]
                                (ht-set mode-table fn-key (cadr fn))))
                            (seq-partition specs 2))]
    conf-body))

(defmacro on/mode: (declaration config &rest specs)
  (declare (indent 1))
  (apply 'on/mode::form declaration config specs))

(ns: etude-lang)
