;;; gptel-extras.el --- Emacs Lisp Indentation -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge
;; URL: https://github.com/ashlineldridge/gptel-extras
;; Version: 0.0.1

;;; Commentary:
;;
;; Some extra commands to support the `gptel' package.

;;; Code:

(defvar gptel-extras-programming-directive
  (concat
   "Act as a highly-skilled software engineer, providing well-researched and "
   "up-to-date answers. Always be transparent about uncertainty: if your "
   "knowledge is incomplete or you are unsure, clearly state it rather than "
   "guess. Refer only to technical constructs (e.g. functions, variables, "
   "libraries, APIs, tools) that verifiably exist; don't invent or extrapolate "
   "without stating so. Favor solutions that align with accepted idiomatic "
   "practices in the relevant technical community. Present code examples as "
   "simply as possible while remaining clear, correct, and sufficient for the "
   "given question. If a question can't be answered with certainty, explain "
   "why and offer partial insights without overstating confidence.")
  "System gptel directive for programming.")

(defvar gptel-extras-chat-buffer-name "*gptel-chat*"
  "Default buffer name used for gptel chat sessions.")

;; Use only the programming directive for now. Need to set both
;; `gptel-directives' and `gptel--system-message' since this package
;; is loaded after `gptel' and `gptel--system-message' is set on load.
(require 'gptel)
(setq
 gptel-directives `((default . ,gptel-extras-programming-directive))
 gptel--system-message gptel-extras-programming-directive)

;;;###autoload
(defun gptel-extras-chat (arg)
  "Switch to or start gptel chat session; select new model if ARG is non-nil."
  (interactive "P")
  (when arg
    (gptel-extras-select-model))
  (display-buffer (gptel (read-buffer
                          "Create or choose gptel buffer: "
                          gptel-extras-chat-buffer-name nil
                          (lambda (b)
                            (and-let* ((buf (get-buffer (or (car-safe b) b))))
                              (buffer-local-value 'gptel-mode buf)))))
                  gptel-display-buffer-action)
  (message "Send your query with %s!"
           (substitute-command-keys "\\[gptel-send]")))

;;;###autoload
(defun gptel-extras-select-model ()
  "Select and update the current gptel model."
  (interactive)
  ;; The following code has been adapted from `gptel--infix-provider' and
  ;; allows the active backend model to be updated via completing-read
  ;; without using the transient-based `gptel-menu'. Consider updating this
  ;; to use consult and call the package consult-gptel?
  (cl-loop
   for (name . backend) in gptel--known-backends
   nconc (cl-loop for model in (gptel-backend-models backend)
                  collect (list (concat name ":" (gptel--model-name model))
                                backend model))
   into models-alist
   with completion-extra-properties =
   `(:annotation-function
     ,(lambda (comp)
	(let* ((model (nth 2 (assoc comp models-alist)))
	       (desc (get model :description))
	       (caps (get model :capabilities))
	       (context (get model :context-window))
	       (input-cost (get model :input-cost))
	       (output-cost (get model :output-cost))
	       (cutoff (get model :cutoff-date)))
	  (when (or desc caps context input-cost output-cost cutoff)
	    (concat
	     (propertize " " 'display `(space :align-to 40))
	     (when desc (truncate-string-to-width desc 70 nil ? t t))
	     " " (propertize " " 'display `(space :align-to 112))
	     (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
	     " " (propertize " " 'display `(space :align-to 134))
	     (when context (format "%5dk" context))
	     " " (propertize " " 'display `(space :align-to 142))
	     (when input-cost (format "$%5.2f in" input-cost))
	     (if (and input-cost output-cost) "," " ")
	     " " (propertize " " 'display `(space :align-to 153))
	     (when output-cost (format "$%6.2f out" output-cost))
	     " " (propertize " " 'display `(space :align-to 166))
	     cutoff)))))
   finally do
   (let ((selected (completing-read "Model: " models-alist nil t)))
     (setq gptel-backend (nth 1 (assoc selected models-alist))
           gptel-model (nth 2 (assoc selected models-alist))))))

(provide 'gptel-extras)
;;; gptel-extras.el ends here
