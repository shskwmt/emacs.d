(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
;;(setq dashboard-startup-banner [VALUE])
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer

;; Content is not centered by default. To center, set
;;(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
;;(setq dashboard-show-shortcuts nil)

(setq dashboard-items '((recents  . 10)
                        (projects . 10)
                        (bookmarks . 5)
                        (agenda . 5)
                        (registers . 5)))
