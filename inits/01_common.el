;; 日本語の設定（UTF-8
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

(setq make-backup-files nil)
(setq delete-auto-save-files t)

;; 対応する括弧をハイライトする
(show-paren-mode 1)

;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

(setq history-length 1000)

;; scroll setting
(setq next-screen-context-lines 30)
(setq scroll-preserve-screen-position t)

(menu-bar-mode -1)

(global-set-key (kbd "C-c g") 'goto-line)
