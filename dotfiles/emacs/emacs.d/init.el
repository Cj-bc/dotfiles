; -*- eval: (leaf-tree-mode 1) -*-
;; Leaf configurations
(eval-and-compile
    (when (or load-file-name byte-compile-current-file)
      (setq user-emacs-directory
          (expand-file-name
	      (file-name-directory (or load-file-name byte-compile-current-file)
	)
	   )
	  )
    )
    )
(setq tab-width 4)
(eval-and-compile
    (customize-set-variable
        'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
		           ("melpa" . "https://melpa.org/packages/")
		           ("org"   . "https://orgmode.org/elpa/")
			   ))
    (package-initialize)
    (unless (package-installed-p 'leaf)
        (package-refresh-contents)
	(package-install 'leaf)
	)

    (leaf leaf-keywords
	  :ensure t
	  :init
	  ;; Optional packages if you want to use :hydra, :el-get, :blackout,,,

	  :init
	  (leaf-keywords-init)
	  )
    )

;; Installing packages
(leaf package-configurations
  :config
  (leaf leaf-tree :ensure t
    )
  (leaf ddskk :ensure t
    :bind
    ("C-x C-j" . skk-mode)
    :config
    (add-to-list 'auto-mode-alist '(".*\\.dict$" . skk-jisyo-edit-mode))
  )
  (leaf howm :ensure t)
  (leaf org
    :hook
    (org-agenda-mode-hook . (lambda () (display-line-numbers-mode -1))
    )
    :config
    (setq org-agenda-files (directory-files-recursively "~/Documents/beorg/howm" "\\.org$"))
    (setq org-enforce-todo-dependencies t)
    (setq org-todo-keywords
  	  '((sequence "TODO" "SOMEDAY" "WAITING(w@)" "|" "DONE(d!)" "OutOfDate")
	    (sequence "ReadLater(a!)" "Reading(i!)" "|" "Read(d!)")
	    (sequence "ToBuy" "Bought(!)")))

    (setq org-link-abbrev-alist
          (("github" . "https://github.com/%s")
            ("youtube" . "https://youtube.com/watch?v=%s")
             ("wikipedia" . "https://en.wikipedia.org/wiki/%s")
                ;; commit, ghFile, twitter, misskeyとかも欲しい
                 ))
    (setq org-clocktable-defaults
          (list :maxlevel 4 :scope agenda :block today :link t :fileskip0 t))
  (org-babel-do-load-languages
     'org-babel-load-languages
      '((Awk . t)))
    )

  )
  )
(leaf twittering-mode :ensure t)
  
;; --- Global settings
(leaf appearences
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
)

(leaf keybinds
  :config
  (leaf window-movements
    :doc "vim-like window movement keybinds"
    :bind
    (("C-c C-w h" . windmove-left)
     ("C-c C-w j" . windmove-down)
     ("C-c C-w k" . windmove-up)
     ("C-c C-w l" . windmove-right)
    )
  )

(leaf configure-global-modes
  :config
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (auto-revert-mode)
)


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes '(tsdh-dark))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages '(twittering-mode leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
