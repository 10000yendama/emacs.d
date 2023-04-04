;;; cspell-flymake.el --- A cpell Flymake backend  -*- lexical-binding: t; -*-
(defvar-local cspell--flymake-proc nil)

(defcustom flymake-cspell-executable nil
  "The path to the cspell executable, used in flymake."
  :group 'flymake
  :type 'string)

(defun cspell-flymake (report-fn &rest _args)

  (unless flymake-cspell-executable
    (error "Path to the executable is not set; \
define `flymake-cspell-executable`."))

  ;; Not having cspell is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  ;;
  (unless (executable-find flymake-cspell-executable)
    (error "Cannot find a suitable cspell"))

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `cspell-flymake-proc' to a different value
  ;;
  (when (process-live-p cspell--flymake-proc)
    (kill-process cspell--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `cspell--flymake-proc' process to a new process
      ;; calling the cspell tool.
      ;;
      (setq
       cspell--flymake-proc
       (make-process
        :name "cspell-flymake" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        ;;
        :buffer (generate-new-buffer " *cspell-flymake*")
        :command (list flymake-cspell-executable (buffer-file-name source))
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          ;;
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `cspell--flymake-proc', which indicates that
                ;; `proc' is not an obsolete process.
                ;;
                (if (with-current-buffer source (eq proc cspell--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's
                      ;; messages and locations, collect them in a list
                      ;; of objects, and call `report-fn'.
                      ;;
                      (cl-loop
                       while (search-forward-regexp
                              ;;             :1        :2            - 3
                              ;; {filename  }:{line   }:{column    } - {Unknown word (word)}
                              "^\\(?:.*\\):\\([0-9]+\\):\\([0-9]+\\) - \\(Unknown word (.*)\\)$"
                              nil t)
                       for msg = (match-string 3)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1))
                                          (string-to-number (match-string 2)))
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        :error
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              ;;
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by
      ;; an EOF.
      ;;
      (process-send-region cspell--flymake-proc (point-min) (point-max))
      (process-send-eof cspell--flymake-proc))))

(defun cspell-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'cspell-flymake nil t))

;; To activate, do:
;;   (add-hook '***-mode-hook 'cspell-setup-flymake-backend)
;; If you are using eglot, it should be:
;;   (add-hook 'eglot-managed-mode-hook
;;             (lambda ()
;;               (when (and (eglot-managed-p) (eq major-mode '***-mode))
;;                 (cspell-setup-flymake-backend))))

(provide 'cspell-flymake)
