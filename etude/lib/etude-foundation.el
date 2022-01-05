(require 'cider)

(defun foundation/debug-last-sexp ()
  (interactive)
  (message (or (cider-last-sexp)
               "<NONE>")))

(defun foundation/cider-eval (code)
  (interactive)
  (message code)
  (cider-interactive-eval
   code
   nil
   nil
   nil))

(defun foundation/cider-eval-at-point (code)
  (interactive)
  (message code)
  (cider-interactive-eval
   code
   (cider-interactive-eval-handler nil (point))
   (cider-last-sexp 'bounds)
   nil))

(defun foundation/make-format-handler ()
  (nrepl-make-response-handler (current-buffer)
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert
                                    (if (derived-mode-p 'cider-clojure-interaction-mode)
                                        (format "\n%s\n" value)
                                      value))
                                   (cider-format-edn-last-sexp)))
                               (lambda (_buffer out)
                                 (cider-emit-interactive-eval-output out))
                               (lambda (_buffer err)
                                 (cider-emit-interactive-eval-err-output err))
                               '()))


(defun foundation/rt-print-module (&optional prefix)
  (interactive)
  (foundation/cider-eval "(std.lang/print-module)"))

(defun foundation/rt-restart ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:restart)"))

(defun foundation/ns-clear ()
  (interactive)
  (save-buffer)
  (foundation/cider-eval "(do (jvm.namespace/clear))"))

(defun foundation/build-triggered ()
  (interactive)
  (save-buffer)
  (cider-eval-buffer)
  (foundation/cider-eval "(std.make/build-triggered)"))

;;
(defun foundation/scaffold-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/create-tests (code.project/source-ns (std.lib/ns-sym)))"))

(defun foundation/import-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/import (code.project/source-ns (std.lib/ns-sym)))"))

;;
;;
;;

(defun foundation/test-setup-global ()
  (interactive)
  (foundation/cider-eval "[(code.test/run:load) (code.test/fact:global :setup)]"))

(defun foundation/test-teardown-global ()
  (interactive)
  (foundation/cider-eval "(code.test/fact:global :teardown)"))

;;
;;
;;

(defun foundation/run-errored-tests ()
  (interactive)
  (foundation/cider-eval "(code.test/run-errored)"))

(defun foundation/run-tests ()
  (interactive)
  (foundation/cider-eval "(code.test/run)"))

(defun foundation/ptr-teardown ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/with:print-all (std.lang/ptr-teardown " (cider-last-sexp) "))")))

(defun foundation/ptr-setup ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/with:print-all (std.lang/ptr-setup " (cider-last-sexp) "))")))

(defun foundation/ptr-emit ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/emit-ptr" (cider-last-sexp) ")")))

(defun foundation/rt-module-purge ()
  (interactive)
  (save-buffer)
  (foundation/cider-eval "(std.lang/rt:module-purge)"))

(defun foundation/ptr-print-or-clip (&optional prefix)
  (interactive "P")
  (if prefix
      (foundation/cider-eval-at-point
       (concat "(std.lang/ptr-print " (cider-last-sexp) ")"))
    (foundation/cider-eval-at-point
     (concat "(std.lang/ptr-clip " (cider-last-sexp) ")"))))

(defun foundation/ptr-redis-print-or-clip (&optional prefix)
  (interactive "P")
  (if prefix
      (foundation/cider-eval-at-point
       (concat "(std.lib/pl (rt.redis/generate-script " (cider-last-sexp) "))"))
    (foundation/cider-eval-at-point
     (concat "(std.lib/clip:nil (rt.redis/generate-script " (cider-last-sexp) "))"))))

(defun foundation/insert-uuid ()
  (interactive)
  (cider-interactive-eval "(str (std.lib/uuid))"
                          (cider-eval-print-handler)
                          nil
                          (cider--nrepl-pr-request-map)))

(defun foundation/wrangle-long-string ()
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    (let ((opoint  (point)))
      (clojure-backward-logical-sexp)
      (kill-region (point) opoint)
      (cider-interactive-eval (concat "(std.string/lines " last-sexp ")")
                              (foundation/make-format-handler)
                              nil
                              (cider--nrepl-pr-request-map)))))

(defun foundation/add-trace ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lib/add-base-trace (var " (cider-last-sexp) "))")))

(defun foundation/add-print-trace ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lib/add-print-trace (var " (cider-last-sexp) "))")))

(defun foundation/add-stack-trace ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lib/add-stack-trace (var " (cider-last-sexp) "))")))

(defun foundation/remove-trace ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lib/remove-trace (var " (cider-last-sexp) "))")))

(defun foundation/get-trace ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lib/get-trace (var " (cider-last-sexp) "))")))

(defun foundation/output-trace ()
  (interactive)
  (cider-interactive-eval (concat "(std.lib/output-trace (var " (cider-last-sexp) "))")
                          (cider-eval-print-handler)
                          nil
                          (cider--nrepl-pr-request-map)))

(defun foundation/trace-ns ()
  (interactive)
  (foundation/cider-eval "(std.lib/trace-ns)"))

(defun foundation/trace-print-ns ()
  (interactive)
  (foundation/cider-eval "(std.lib/trace-print-ns)"))

(defun foundation/untrace-ns ()
  (interactive)
  (foundation/cider-eval "(std.lib/untrace-ns)"))

(defun foundation/paste-string ()
  (interactive)
  (cider-interactive-eval "(std.lib/paste)"
                          (cider-eval-print-handler)
                          nil
                          (cider--nrepl-pr-request-map)))

(defun foundation/print-or-clip-last-expr (&optional prefix)
  (interactive)
  (if prefix
      (foundation/cider-eval-at-point
       (concat "(std.lib/prf " (cider-last-sexp) ")"))
    (foundation/cider-eval-at-point
     (concat "(std.lib/clip:nil " (cider-last-sexp) ")"))))

(defun foundation/paste-long-string ()
  (interactive)
  (cider-interactive-eval "(std.string/lines (std.lib/paste))"
                          (foundation/make-format-handler)
                          nil
                          (cider--nrepl-pr-request-map)))

(defun foundation/paste-format ()
  (interactive)
  (cider-interactive-eval "(read-string (std.lib/paste))"
                          (foundation/make-format-handler)
                          nil
                          (cider--nrepl-pr-request-map)))

(provide 'etude-foundation)
