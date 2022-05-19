(require 'dap-mode)
(require 'dash)
(require 'f)

;; Debug adapter for:
;; https://github.com/grahambates/uae-dap

;; TODO: automatic download of debug adapter.
;;       looks like dap-utils--get-extension is a way to download. Any npm ways?
;;       just use lsp-dependency for this as well? and package lsp-package-ensure in the setup function? 

(defcustom dap-uae-fs-uae-path (executable-find "fs-uae")
  "Path to FS UAE executable. Should be a patch version that supports debugging. https://github.com/prb28/vscode-amiga-assembly-binaries"
  :group 'dap-uae
  :type 'string)

;; TODO: some of the parameters is probably better to put in a template?
;; some sane-ish defaults
(defun dap-uae-configure-parameters (conf)
  (-> conf
      (dap--put-if-absent :type "asm68k"
                          :request launch
                          :program (expand-file-name (read-file-name "Select executable file to debug"))
                          :cwd (lsp-workspace-root)
                          :stopOnEntry :json-false
                          :serverName "localhost"
                          :serverPort "6860"
                          :trace :json-false
                          :startEmulator t
                          :emulator dap-uae-fs-uae-path 
                          :emulatorWorkingDir (f-dirname dap-uae-fs-uae-path)
                          :emulatorOptions ["--hard_drive_0=uae/dh0"
                                            "--remote_debugger=200"
                                            "--use_remote_debugger=true"
                                            "--automatic_input_grab=0"])))

(dap-register-debug-provider "asm68k" #'dap-uae-configure-parameters)

;; TODO: a template with basic settings? maybe where we can select the program file?
