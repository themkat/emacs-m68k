;;; m68k-mode.el --- Major mode and utilities for M68k assembly programming

;; URL: https://github.com/themkat/emacs-m68k

;; Package-Requires: ((emacs "25.1") (lsp-mode "8.0.0") (dap-mode "0.7") (tree-sitter "0.12.0"))
;; Version: 0.0.1

;;; Commentary:
;; Various utilities for programming M68k assembly. This includes lsp, debugging and more.

;;; Code:

(define-derived-mode m68k-mode
  asm-mode "m68k-mode"
  "Mode for programming Motorola 68k processors with automatic lsp activation")

;; tree sitter for better syntax highlight
;; requires https://github.com/grahambates/tree-sitter-m68k
;; TODO: setup method for tree-sitter-m68k?
;; TODO: any way we can make this optional if grammar not installed?
;;(add-to-list 'tree-sitter-major-mode-language-alist '(m68k-mode . m68k))

(defcustom m68k-tree-sitter-enabled nil
  "Turns on tree-sitter mode for better syntax highlighting. Requires you to set up tree-sitter-m68k and tree-sitter mode."
  :type 'string
  :group 'm68k-mode)

;; TODO: other utilities?

(defun m68k-setup-tree-sitter ()
  "Tree sitter setup."
  (when m68k-tree-sitter-enabled
    (require 'tree-sitter)
    (tree-sitter-hl-mode 1)))

(add-to-list 'm68k-mode-hook #'m68k-setup-tree-sitter)


(require 'lsp-m68k)
;;(require 'dap-m68k)

(provide 'm68k-mode)
;;; m68k-mode.el ends here
