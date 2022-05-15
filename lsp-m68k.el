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

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection)
  :priority -1
  :activation-fn (lsp-activate-on "m68k")
  :server-id 'm68k-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'm68k-lsp-server callback error-callback))))
