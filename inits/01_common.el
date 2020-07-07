(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(setq delete-auto-save-files t)

(show-paren-mode 1)

(savehist-mode 1)

(line-number-mode 1)
(column-number-mode 1)
(add-hook 'find-file-hooks 'linum-mode)

(add-hook 'find-file-hooks 'whitespace-mode)

(setq history-length 1000)

;; scroll setting
(setq next-screen-context-lines 30)
(setq scroll-preserve-screen-position t)

(menu-bar-mode -1)

(global-set-key (kbd "C-c g") 'goto-line)
