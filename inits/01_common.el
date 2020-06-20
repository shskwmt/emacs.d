;; 日本語の設定（UTF-8
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 対応する括弧をハイライトする
(show-paren-mode 1)

;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

;; 履歴を保存する
(setq history-length 1000)
