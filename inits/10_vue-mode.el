(add-hook 'vue-mode-hook
          (lambda ()
            (make-local-variable 'typescript-indent-level)
            (setq typescript-indent-level 2)
            (setq css-indent-offset 2)
            (setq pug-tab-width 2)))
