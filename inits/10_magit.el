(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame)

(with-eval-after-load "magit"
  (setq magit-completing-read-function 'ivy-completing-read))
