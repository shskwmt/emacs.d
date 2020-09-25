(add-hook 'typescript-mode-hook
          (lambda ()
            (make-local-variable 'typescript-indent-level)
            (setq typescript-indent-level 2)))
