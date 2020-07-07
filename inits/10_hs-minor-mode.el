;; hs-minor-mode
(define-key global-map (kbd "C-\\") 'hs-toggle-hiding)

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (hs-minor-mode 1)))
(add-hook 'rust-mode-hook
	  '(lambda ()
	     (hs-minor-mode 1)))
