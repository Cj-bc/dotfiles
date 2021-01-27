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
(leaf leaf-tree :ensure t
  :config
  (add-to-list 'auto-mode-alist '("init.el" . leaf-tree-mode))
  )
(leaf ddskk :ensure t
  :bind
  ("C-x C-j" . skk-mode)

  :config
  (add-to-list 'auto-mode-alist '(".*\\.dict$" . skk-jisyo-edit-mode))
)
  
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )