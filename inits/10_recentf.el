(require 'recentf-ext)

(setq recentf-max-saved-items 1000)

(run-with-idle-timer 30 t '(lambda ()
   (with-suppressed-message (recentf-save-list))))

