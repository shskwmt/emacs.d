(require 'popwin)

(defvar cursor-history-list nil)
(defvar cursor-history-last-visited-buffer-info nil)
(defvar cursor-history-buffer "*cursor-history*")
(defvar cursor-history-last-previewed-buffer nil)

(defcustom cursor-history-max-item-num 500
  "Max number of cursors saved in history."
  :type 'integer
  :group 'cursor-history)

(defcustom cursor-history-buffer-height 30
  "Buffer height to show cursor-history."
  :type 'integer
  :group 'cursor-history)

(defcustom cursor-history-save-timer 1
  "Interval time to save cursor in history."
  :type 'integer
  :group 'cursor-history)

(defcustom cursor-history-ignore-buffer "^ \\*Minibuf\\|^ \\*cursor-history*"
  "Regular expression describing buffer names that are never saved in history."
  :type '(choice (const nil) (regexp))
  :group 'cursor-history)

(defcustom cursor-history-ignore-major-mode nil
  "List of ignored major modes."
  :type '(repeat symbol)
  :group 'cursor-history)

(defcustom cursor-history-should-preview t
  "If non-nil, it shows the preview of buffers at the cursor in `cursor-history-buffer'."
  :type 'boolean
  :group 'cursor-history)

(defvar cursor-history-show-mode-map
  (let ((kmap (make-sparse-keymap)))
    ;; point-history-goto
    (define-key kmap (kbd "RET")     'cursor-history-goto)
    ;; point-history-next-line
    (define-key kmap (kbd "n")       'cursor-history-next-line)
    (define-key kmap (kbd "C-n")     'cursor-history-next-line)
    (define-key kmap (kbd "TAB")     'cursor-history-next-line)
    ;; point-history-prev-line
    (define-key kmap (kbd "p")       'cursor-history-prev-line)
    (define-key kmap (kbd "C-p")     'cursor-history-prev-line)
    (define-key kmap (kbd "<C-tab>") 'cursor-history-prev-line)
    ;; point-history-close
    (define-key kmap (kbd "g")       'cursor-history-close)
    (define-key kmap (kbd "C-g")     'cursor-history-close)
    kmap))

(defun cursor-history-next-line ()
  "Go to next line in `cursor-history-show-mode'.
If the current line number is end of the buffer, go to the first line."
  (interactive)
  (let* ((current-line-num (line-number-at-pos))
         (begining-line-num 1)
         (total-line-num (count-lines (point-min) (point-max))))
    (if (>= current-line-num total-line-num)
        (goto-line begining-line-num)
      (goto-line (+ 1 current-line-num)))
    (if cursor-history-should-preview
        (cursor-history-preview-at-point))))

(defun cursor-history-prev-line ()
  "Go to previous line in `cursor-history-show-mode'.
If the current line number is begining of the buffer, go to the last line."
  (interactive)
  (let* ((current-line-num (line-number-at-pos))
         (begining-line-num 1)
         (total-line-num (count-lines (point-min) (point-max))))
    (if (<= current-line-num begining-line-num)
        (goto-line total-line-num)
      (goto-line (- current-line-num 1)))
    (if cursor-history-should-preview
        (cursor-history-preview-at-point))))

(defun cursor-history-goto ()
  "Go to the cursor in cursor-history list."
  (interactive)
  (let* ((buffer-str (get-text-property (point) 'cursor-history-buffer))
         (pos-str (get-text-property (point) 'cursor-history-position))
         (pos (string-to-number pos-str)))
    (if (null buffer-str)
        (message "No point at this line.")
      (cursor-history--goto buffer-str pos))))

(defun cursor-history--goto (buffer-str pos)
  "Pop to BUFFER-STR and go to POS, then close `cursor-history-buffer'."
  (with-selected-window (get-buffer-window cursor-history-last-previewed-buffer)
    (goto-char pos)
    (cursor-history-close t)))

(defun cursor-history-close (&optional only-close)
  "Close cursor-history-buffer.
If ONLY-CLOSE is non-nil, it dose not call `pop-to-buffer'."
  (interactive)
  (if only-close
      (popwin:close-popup-window)
    (with-selected-window (get-buffer-window cursor-history-last-previewed-buffer)
      (let* ((buffer (nth 0 cursor-history-last-visited-buffer-info))
             (pos (nth 1 cursor-history-last-visited-buffer-info)))
        (pop-to-buffer-same-window buffer)
        (goto-char pos)
        (popwin:close-popup-window))))
  (kill-buffer cursor-history-buffer))

(defun cursor-history--remove-duplicate-element (content buffer line-num)
  "Remove duplicated element by comparing CONTENT & BUFFER & LINE-NUM."
  (seq-remove (lambda (elt)
                (and (string-equal (substring-no-properties (nth 1 elt)) content)
                     (eq (nth 2 elt) line-num)
                     (string-equal (buffer-name (nth 3 elt)) (buffer-name buffer))))
              cursor-history-list))

(defun cursor-history--maybe-unique-push! (item)
  "Push ITEM into re-builded point-history-list containing maybe unique elements."
  (let* ((content (substring-no-properties (nth 1 item)))
         (line-num (nth 2 item))
         (buffer (nth 3 item))
         (maybe-unique-element-list
          (cursor-history--remove-duplicate-element
           content buffer line-num))
         (unique-element-list
          (delq nil (delete-dups maybe-unique-element-list))))
    (setq cursor-history-list unique-element-list)
    (push item cursor-history-list)))

(defun cursor-history--save-last-visited-buffer! (buffer)
  "Set BUFFER as `cursor-history-last-visited-buffer-info'."
  (let* ((buffer-info buffer)
         (pos-info (point))
         (info (list buffer-info pos-info)))
    (setq cursor-history-last-visited-buffer-info info)))

(defun cursor-history--save-last-previewed-buffer! (buffer)
  "Set BUFFER as `cursor-history-last-previewed-buffer'."
  (setq cursor-history-last-previewed-buffer buffer))

(defun cursor-history--build-valid-buffer-list! ()
  "Remove killed buffer from cursor-history-list."
  (let* ((old-cursor-history-list cursor-history-list)
         (new-cursor-history-list
          (seq-filter (lambda (elt) (buffer-live-p (nth 3 elt)))
                      old-cursor-history-list)))
    (setq cursor-history-list new-cursor-history-list)))

(defun cursor-history--build-history (points)
  "Build human readable history-list from POINTS."
  (dolist (point points)
    (let* ((pos-info (format "%s" (marker-position (nth 0 point))))
           (buffer-info (format "%s" (marker-buffer (nth 0 point))))
           (content-info (format "%s" (nth 1 point)))
           (str (concat buffer-info ": " pos-info ": " content-info)))
      (put-text-property 0 (length str) 'cursor-history-buffer buffer-info str)
      (put-text-property 0 (length str) 'cursor-history-position pos-info str)
      (insert (concat str "\n"))))
  (setq header-line-format header)
  (setq buffer-read-only t)
  (beginning-of-buffer))

(defun cursor-history-preview-at-point ()
  "Preview the buffer at point in the other window."
  (interactive)
  (let* ((buffer-str (get-text-property (point) 'cursor-history-buffer))
         (buffer (get-buffer buffer-str))
         (pos-str (get-text-property (point) 'cursor-history-position))
         (pos (string-to-number pos-str)))
    (cursor-history--preview-at-point buffer pos)
    (popwin:select-popup-window)))

(defun cursor-history--preview-at-point (buffer pos)
  "Show BUFFER and set the cursor at POS."
  (with-selected-window (get-buffer-window cursor-history-last-previewed-buffer)
    (pop-to-buffer-same-window buffer)
    (goto-char pos)
    (cursor-history--save-last-previewed-buffer! buffer)))

(defun cursor-history--push-item! (item)
  "Push ITEM to cursor-history-list."
  (cursor-history--maybe-unique-push! item)
  (cursor-history--build-valid-buffer-list!)
  (if (> (length cursor-history-list) cursor-history-max-item-num)
      (let* ((last-item (car (last cursor-history-list)))
             (new-cursor-history--list (remove last-item cursor-history-list)))
        (setq cursor-history-list new-cursor-history--list))))

(defun cursor-history--update-list! ()
  "Build cursor-history to push cursor-history-list."
  (let* ((marker (point-marker))
         (buffer (marker-buffer marker))
         (line-num (line-number-at-pos))
         (line-content (buffer-substring
                        (line-beginning-position) (line-end-position)))
         (point-item (list marker line-content line-num buffer)))
    (if (and (not (string-match-p cursor-history-ignore-buffer (buffer-name buffer)))
             (not (member major-mode cursor-history-ignore-major-mode)))
        ;; Each point-item has
        ;; nth 0: marker info
        ;; nth 1: line content
        ;; nth 2: line number
        ;; nth 3: buffer info
        (cursor-history--push-item! point-item))))

(defun cursor-history-show-mode nil
  "Major mode for `cursor-history-show-mode' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cursor-history-show-mode)
  (setq mode-name "cursor-history-show")
  (use-local-map cursor-history-show-mode-map))

(defun cursor-history--show ()
  "Open the list buffer of point-history-list."
  (let* ((cursors cursor-history-list)
         (header "buffer:linum:content"))
    (cursor-history--save-last-visited-buffer! (current-buffer))
    (cursor-history--save-last-previewed-buffer! (current-buffer))
    (popwin:popup-buffer
     (generate-new-buffer cursor-history-buffer)
     :stick t
     :height cursor-history-buffer-height)
    (cursor-history-show-mode)
    (cursor-history--build-history cursors)
    (cursor-history-preview-at-point)))

(defun cursor-history-show ()
  "Show cursor-history-list."
  (interactive)
  (cursor-history--show))

;;;###autoload
(define-minor-mode cursor-history-mode
  "Global minor mode for cursor-history-mode"
  :init-value nil
  :global t
  :lighter " ch"
  (if cursor-history-mode
      (setq cursor-history-timer
            (run-with-idle-timer
             cursor-history-save-timer t 'cursor-history--update-list!))
    (cancel-timer cursor-history-timer)))


;; enable minor mode
(cursor-history-mode t)
(global-set-key (kbd "C-c s") 'cursor-history-show)
