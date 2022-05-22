
;; Package-Requires: ((emacs "25.1") (lsp-mode "8.0.0") (tree-sitter "0.12.0"))

(define-derived-mode m68k-mode
  asm-mode "m68k-mode"
  "Mode for programming Motorola 68k processors with automatic lsp activation")


;; tree sitter for better syntax highlight
;; requires https://github.com/grahambates/tree-sitter-m68k
;; TODO: setup method for tree-sitter-m68k?
;; TODO: any way we can make this optional if grammar not installed?
(add-to-list 'tree-sitter-major-mode-language-alist '(m68k-mode . m68k))

(defcustom m68k-tree-sitter-enabled nil
  "Turns on tree-sitter mode for better syntax highlighting. Requires you to set up tree-sitter-m68k and tree-sitter mode.")

;; TODO: other utilities?

(defun m68k-setup-tree-sitter ()
  (when m68k-tree-sitter-enabled
    (require 'tree-sitter)
    (tree-sitter-hl-mode 1)))

(add-to-list 'm68k-mode-hook #'m68k-setup-tree-sitter)


(require 'lsp-m68k)
;;(require 'dap-m68k)

(provide 'm68k-mode)
