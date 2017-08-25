;; 日本語の設定（UTF-8）
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; 現在行を目立たせる
(global-hl-line-mode)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
