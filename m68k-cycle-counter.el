;;; m68k-cycle-counter.el --- Minor mode to get cycle counting overlays

;; URL: https://github.com/themkat/emacs-m68k

;; Package-Requires: ((emacs "25.1") (dash "2.19.1") (s "1.13.0"))
;; Version: 0.0.1

;;; Commentary:
;; Cycle counter overlays for m68k-mode, using:
;; https://github.com/grahambates/68kcounter

;;; Code:
(require 'dash)

;; TODO: some periodic update of overlays. Maybe just when the user saves? To get some information?

(defcustom m68k-cycle-counter-update-interval 2.5
  "Interval between each cycle counter update."
  :type 'float
  :group 'm68k-cycle-counter)

;; TODO: maybe a tweakable value threshold before we show warning on cycles? red=warning?

(defcustom m68k-cycle-counter-overlay-max-length 35
  "Maximum length of the cycle counter overlays"
  :type 'number
  :group 'm68k-cycle-counter)


(defface m68k-cycle-counter-overlay-face
  '((t (:inherit highlight :box t :extend nil)))
  "Face used in m68k cycle counter overlays"
  :group 'm68k-cycle-counter)

(defvar m68k-cycle-counter-overlays nil)
(defvar m68k-cycle-counter-timer nil)

(defun m68k-cycle-counter-clear-overlays ()
  "Delete only m68k-cycle-counter overlays, not all overlays in buffer."
  (-each m68k-cycle-counter-overlays
    (lambda (ov)
      (delete-overlay ov)))
  (setq m68k-cycle-counter-overlays '()))

;; TODO: propertize based upon the number of cycles. Maybe an error face or similar could be set for very high cycle counts?
(defun m68k-cycle-counter--propertize-text (text cycles)
  "Makes the text conform to set width and styling depending on cycle count."
  (let ((str (string-pad text m68k-cycle-counter-overlay-max-length)))
    (add-face-text-property 0 m68k-cycle-counter-overlay-max-length
                            'm68k-cycle-counter-overlay-face nil str)
    str))

(defun m68k-cycle-counter-create ()
  ;; TODO: could probably do more error checking for ouput length + length of buffer
  (let* ((data (shell-command-to-string (string-join `("npx 68kcounter " ,(buffer-file-name) " -c=false -w=20 --include 'text,timings,bytes'"))))
         (counter-info (-map (lambda (x)
                               (let* ((info-column (s-trim (car (s-slice-at "|" x))))
                                      (matches-multiple (s-match "^\\([0-9]+\\)(\\([0-9]+\\)/\\([0-9]+\\)) \\([0-9]+\\)" info-column))
                                      (matches-single-num (s-match "^\\([0-9]+\\)" info-column)))
                                 (cond (matches-multiple
                                        (list (cadr matches-multiple)
                                              (caddr matches-multiple)
                                              (cadddr matches-multiple)
                                              (cadddr (cdr matches-multiple))))
                                       (matches-single-num
                                        (list (cadr matches-single-num)))
                                       (t
                                        nil))
                                 ))
                             (s-lines data))))
    ;; delete the old overlays before drawing new ones
    (m68k-cycle-counter-clear-overlays)

    (setq-local m68k-cycle-counter-overlays '())
    
    (save-excursion
      (goto-line 1)
      (-each counter-info
        (lambda (entry)
          (let ((overlay (make-overlay (line-beginning-position)
                                       (line-end-position))))
            (overlay-put overlay 'priority 1)
            (-if-let ((cycles reads writes size)
                      entry)
                (overlay-put overlay 'before-string
                             (m68k-cycle-counter--propertize-text (format "Cycles: %3s (R:%2s/W:%2s) Size: %2s"
                                                                          cycles reads writes size)
                                                                  cycles))
              (-if-let ((size)
                        entry)
                  (overlay-put overlay 'before-string (m68k-cycle-counter--propertize-text (format "Size: %2s" size) 0))
                (overlay-put overlay 'before-string (string-pad "" m68k-cycle-counter-overlay-max-length))))
            (push overlay m68k-cycle-counter-overlays))

          (forward-line 1))))
    
    ;; run this again on the update interval
    ;; TODO
    ))

(define-minor-mode m68k-cycle-counter-mode
  "Minor mode for viewing cycle counter data as overlays."
  :group 'm68k-cycle-counter
  :lighter "M68K Cycle Counter"
  ;; TODO: activate the cycle counter
  (if m68k-cycle-counter-mode
      (m68k-cycle-counter-create)
    (m68k-cycle-counter-clear-overlays)))

(provide 'm68k-cycle-counter)
;;; m68k-cycle-counter.el ends here
