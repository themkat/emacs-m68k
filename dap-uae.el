;;; dap-uae.el --- dap-mode config for m68k assembly using uae-dap

;;; Commentary:
;; Debug adapter for:
;; https://github.com/grahambates/uae-dap

;;; Code:

(require 'dap-mode)
(require 'dash)
(require 'f)

;; super simple setup function
(defun m68k-dap-uae-setup ()
  (interactive)
  (if (executable-find "npm")
      (shell-command "npm install -g uae-dap")
    (error "npm not found")))

(defcustom m68k-dap-uae-fs-uae-path (executable-find "fs-uae")
  "Path to FS UAE executable. Should be a patch version that supports debugging. https://github.com/prb28/vscode-amiga-assembly-binaries"
  :group 'dap-uae
  :type 'string)

;; TODO: some of the parameters is probably better to put in a template?
;; some sane-ish defaults
(defun m68k-dap-uae-configure-parameters (conf)
  (-> conf
      (dap--put-if-absent :type "asm68k")
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :dap-server-path "dap-uae")
      (dap--put-if-absent :program (expand-file-name (read-file-name "Select executable file to debug")))
      (dap--put-if-absent :cwd (lsp-workspace-root))
      (dap--put-if-absent :stopOnEntry :json-false)
      (dap--put-if-absent :serverName "localhost")
      (dap--put-if-absent :serverPort "6860")
      (dap--put-if-absent :trace :json-false)
      (dap--put-if-absent :startEmulator t)
      (dap--put-if-absent :emulator m68k-dap-uae-fs-uae-path)
      (dap--put-if-absent :emulatorWorkingDir (f-dirname m68k-dap-uae-fs-uae-path))
      ;; TODO: fix. Causes bytecode overflow it seems....
      ;; (dap--put-if-absent :emulatorOptions ["--hard_drive_0=uae/dh0"
      ;;                                       "--remote_debugger=200"
      ;;                                       "--use_remote_debugger=true"
      ;;                                       "--automatic_input_grab=0"])
      ))

(dap-register-debug-provider "asm68k" #'m68k-dap-uae-configure-parameters)

;; TODO: a template with basic settings? maybe where we can select the program file?

(provide 'dap-uae)
;;; dap-uae.el ends here
