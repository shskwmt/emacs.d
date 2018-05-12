;; パッケージの設定
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; 日本語の設定（UTF-8
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; 現在行を目立たせる
;;(global-hl-line-mode 1)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)

;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;; 対応する括弧をハイライトする
(show-paren-mode 1)

;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

;; 履歴を保存する
(setq history-length 1000)

;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; helmの設定
(require 'helm-config)
(helm-mode 1)

;; magit
(require 'magit)

;; neotree
(require 'neotree)
(setq neo-show-hidden-files t)
(setq neo-create-file-auto-open t)
(setq neo-persist-show t)
;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)

;; for ruby
(require 'rspec-mode)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
