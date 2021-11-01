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

(defun foundation/rt-setup (&optional prefix)
  (interactive)
  (foundation/cider-eval "(std.lang/rt:setup)"))

(defun foundation/rt-teardown (&optional prefix)
  (interactive)
  (foundation/cider-eval "(std.lang/rt:teardown)"))

(defun foundation/rt-resetup (&optional prefix)
  (interactive)
  (foundation/cider-eval "(do (std.lang/rt:teardown) (std.lang/rt:setup))"))

(defun foundation/rt-reprep ()
  (interactive)
  (foundation/cider-eval "(do (std.lang/rt:teardown) (std.lang/rt:module-prep))"))

(defun foundation/rt-emit-module (&optional prefix)
  (interactive)
  (foundation/cider-eval "(std.lang/emit-module)"))

(defun foundation/rt-refresh ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:refresh)"))

(defun foundation/rt-restart ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:inner-restart)"))

(defun foundation/ns-reeval ()
  (interactive)
  (save-buffer)
  ;; (foundation/cider-eval "(do (std.lang/ns:reset))")
  (foundation/cider-eval "(do (./clear))")
  )

(defun foundation/ns-rebuild ()
  (interactive)
  (save-buffer)
  (cider-eval-buffer)
  (foundation/cider-eval "(std.lib/make:build-roots)"))
;;
(defun foundation/scaffold-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/create-tests [(code.project/source-ns (std.lib/ns-sym))])"))

(defun foundation/import-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/import [(code.project/source-ns (std.lib/ns-sym))])"))

(defun foundation/incomplete-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/incomplete [(code.project/source-ns (std.lib/ns-sym))])"))

(defun foundation/orphaned-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/orphaned [(code.project/source-ns (std.lib/ns-sym))])"))

(defun foundation/pedantic-tests ()
  (interactive)
  (foundation/cider-eval
   "(code.manage/pedantic [(code.project/source-ns (std.lib/ns-sym))])"))

(defun foundation/run-errored-tests ()
  (interactive)
  (foundation/cider-eval "(code.test/run-errored)"))

(defun foundation/run-tests ()
  (interactive)
  (foundation/cider-eval "(code.test/run)"))

(defun foundation/test-setup-global ()
  (interactive)
  (foundation/cider-eval "[(code.test/run:load) (code.test/fact:global :setup)]"))

(defun foundation/test-teardown-global ()
  (interactive)
  (foundation/cider-eval "(code.test/fact:global :teardown)"))

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

(defun foundation/ptr-print-or-clip (&optional prefix)
  (interactive "P")
  (if prefix
      (foundation/cider-eval-at-point
       (concat "(std.lang/ptr-print " (cider-last-sexp) ")"))
    (foundation/cider-eval-at-point
     (concat "(std.lang/ptr-clip " (cider-last-sexp) ")"))))

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
      (cider-interactive-eval (concat "(./lines " last-sexp ")")
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
                                                           '())
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

(provide 'etude-foundation)
