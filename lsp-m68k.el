
;; Package-Requires: ((emacs "25.1") (lsp-mode "8.0.0") (tree-sitter "0.12.0"))

;; TODO: maybe separate the different concerns from this file into their own files?

;; lsp mode configuration for
;; https://github.com/grahambates/m68k-lsp

(require 'lsp-mode)

;; TODO: should we make a separate mode for it? or something else? m68k-mode?
(define-derived-mode m68k-mode
  asm-mode "m68k-mode"
  "Mode for programming Motorola 68k processors with automatic lsp activation")

(add-to-list 'm68k-mode-hook #'lsp)
(add-to-list 'lsp-language-id-configuration '(m68k-mode . "m68k"))

(lsp-dependency
 'm68k-lsp-server
 '(:system "m68k-lsp-server")
 '(:npm :package "m68k-lsp-server"
        :path "m68k-lsp-server"))

;; TODO: how many of the configuration options should we add? All? 
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
