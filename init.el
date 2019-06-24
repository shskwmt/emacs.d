;; for straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

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
