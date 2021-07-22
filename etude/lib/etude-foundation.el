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
   nil
   nil))

(defun foundation/rt-setup ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:setup)"))

(defun foundation/rt-teardown ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:teardown)"))

(defun foundation/rt-resetup ()
  (interactive)
  (foundation/cider-eval "(do (std.lang/rt:teardown) (std.lang/rt:setup))"))

(defun foundation/rt-reprep ()
  (interactive)
  (foundation/cider-eval "(do (std.lang/rt:teardown) (std.lang/rt:module-prep))"))

(defun foundation/rt-load ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:load)"))

(defun foundation/rt-unload ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:unload)"))

(defun foundation/rt-refresh ()
  (interactive)
  (foundation/cider-eval "(std.lang/rt:refresh)"))

(defun foundation/ns-reeval ()
  (interactive)
  (save-buffer)
  (foundation/cider-eval
   "(do (std.lang/ns:reset) nil)")
  (cider-eval-buffer))

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
  
(defun foundation/ptr-teardown ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/with:print (std.lang/ptr-teardown " (cider-last-sexp) "))")))

(defun foundation/ptr-setup ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/with:print (std.lang/ptr-setup " (cider-last-sexp) "))")))

(defun foundation/ptr-emit ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/ptr-emit " (cider-last-sexp) ")")))

(defun foundation/ptr-print ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/ptr-print " (cider-last-sexp) ")")))

(defun foundation/ptr-clip ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/ptr-clip " (cider-last-sexp) ")")))

(defun foundation/ptr-print ()
  (interactive)
  (foundation/cider-eval-at-point
   (concat "(std.lang/ptr-print " (cider-last-sexp) ")")))

(provide 'etude-foundation)
