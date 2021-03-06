;;; lsp-m68k.el --- lsp-mode configuration for m68k assembly

;;; Commentary:
;; Various utilities for programming M68k assembly. This includes lsp, debugging and more.

;;; Code:


;; TODO: maybe separate the different concerns from this file into their own files?

;; lsp mode configuration for
;; https://github.com/grahambates/m68k-lsp

(require 'lsp-mode)

;; defined in m68k-mode
(defvar m68k-mode-hook nil)

(add-to-list 'm68k-mode-hook #'lsp)
(add-to-list 'lsp-language-id-configuration '(m68k-mode . "m68k"))

(lsp-dependency
 'm68k-lsp-server
 '(:system "m68k-lsp-server")
 '(:npm :package "m68k-lsp-server"
        :path "m68k-lsp-server"))

;; TODO: how many of the configuration options should we add
(defcustom m68k-processor-types ["mc68000"]
  "Types of processors we are targeting (e.g, mc68000, mc68020, mc68881 etc.). Set to get fitting completion candidates, documentation etc."
  :group 'm68k
  :type 'vector)

(lsp-register-custom-settings
 '(("m68k.format.case" "lower")
   ("m68k.processors" m68k-processor-types)))


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (list "m68k-lsp-server" "--stdio")))
  :priority -1
  :activation-fn (lsp-activate-on "m68k")
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "m68k"))))
  :server-id 'm68k-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'm68k-lsp-server callback error-callback))))

(provide 'lsp-m68k)
;;; lsp-m68k.el ends here
