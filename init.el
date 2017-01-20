
(setq package-archives
      '(
	
	;;("melpa" . "https://melpa.milkbox.net/packages/")
	;;("popkit" . "http://elpa.popkit.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	
	;; ("org"       . "http://orgmode.org/elpa/")
	("gnu"       . "http://elpa.gnu.org/packages/")

	;; ("marmalade" .  "http://marmalade-repo.org/packages/")

    ))

(package-initialize)
(load-file "pjx.el")
(setq pjx-root-directory "~/Document/projects")




