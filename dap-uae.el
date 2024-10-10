;;; dap-uae.el --- dap-mode config for m68k assembly using uae-dap

;;; Commentary:
;; Debug adapter for:
;; https://github.com/grahambates/uae-dap

;;; Code:

(require 'dap-mode)

;; super simple setup function
(defun m68k-dap-uae-setup ()
  (interactive)
  (if (executable-find "npm")
      (shell-command "npm install -g uae-dap")
    (error "npm not found")))

;; TODO: reinstroduce some configurability on executable
;; (defcustom m68k-dap-uae-fs-uae-path (executable-find "fs-uae")
;;   "Path to FS UAE executable. Should be a patch version that supports debugging. https://github.com/prb28/vscode-amiga-assembly-binaries"
;;   :group 'dap-uae
;;   :type 'string)

(defcustom m68k-compile-command nil
  "Command used to compile project before debugging. If nil, you would be responsible for compilation yourself.
Pro-tip: Use a .dir-locals setting for this per project if unique."
  :type 'string
  :group 'dap-uae)

(defun m68k-dap-uae-configure-parameters (conf)
  (-> conf
      (dap--put-if-absent :name "Amiga ASM")
      (dap--put-if-absent :type "asm68k")
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :dap-server-path (list "uae-dap"))
      ;; (when m68k-compile-command
      ;;   (dap--put-if-absent :dap-compilation m68k-compile-command))
      (dap--put-if-absent :program (read-file-name "Select executable file to debug"))
      (dap--put-if-absent :cwd (lsp-workspace-root))
      (dap--put-if-absent :serverName "localhost")
      (dap--put-if-absent :serverPort "2345")
      (dap--put-if-absent :stopOnEntry t)
      (dap--put-if-absent :enableJsonLogging t)
      ;; some sensible default args
      (dap--put-if-absent :emulatorArgs ["--chip_memory=2048"
                                         "--amiga_model=A1200"
                                         "--automatic_input_grab=0"
                                         "--floppy_drive_0_sounds=off"
                                         "--hide_hud=1"
                                         "--window_resizable=1"])))

(dap-register-debug-provider "asm68k" #'m68k-dap-uae-configure-parameters)

;; Basic debug template
(dap-register-debug-template
 "Amiga ASM debug"
 (list :type "asm68k"
       :request "launch"))

(provide 'dap-uae)
;;; dap-uae.el ends here
