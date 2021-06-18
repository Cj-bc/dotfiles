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
  (leaf org
    :hook
    (org-agenda-mode-hook . (lambda () (display-line-numbers-mode -1))
    )
    :custom
    (org-global-properties . '(("TASK_KIND_ALL" . "やること 勉強 休憩 生活")))
    (org-clock-clocktable-default-properties . '(:maxlevel 4 :scope agenda :block today :link t :fileskip0 t))
    (org-agenda-clockreport-parameter-plist . org-clock-clocktable-default-properties)
    (org-enforce-todo-dependencies . t)
    (org-todo-keywords .
  	  '((sequence "TODO" "SOMEDAY" "WAITING(w@)" "|" "DONE(d!)" "OutOfDate")
	    (sequence "ReadLater(a!)" "Reading(i!)" "|" "Read(d!)")
	    (sequence "ToBuy" "Bought(!)")))
    (org-link-abbrev-alist .
  	  '(("github" . "https://github.com/%s")
  	    ("youtube" . "https://youtube.com/watch?v=%s")
  	    ("wikipedia" . "https://en.wikipedia.org/wiki/%s")
  	    ("archw" . "https://wiki.archlinux.jp/index.php/%s")
  	    ;; commit, ghFile, twitter, misskeyとかも欲しい
  	    ))
    (org-latex-compiler . "xelatex")

    :config
    (setq org-agenda-files (directory-files-recursively "~/Dropbox" "^[^#].\+\\.org$"))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((awk . t)
       (shell . t)
       (haskell . t)
       (plantuml . t)))

    (leaf org-roam
      :doc "Roam Research replica with Org-mode"
      :req "emacs-26.1" "dash-2.13" "f-0.17.2" "s-1.12.0" "org-9.3" "emacsql-3.0.0" "emacsql-sqlite3-1.0.2"
      :tag "convenience" "roam" "org-mode" "emacs>=26.1"
      :added "2021-05-18"
      :url "https://github.com/org-roam/org-roam"
      :emacs>= 26.1
      :ensure t
      :custom
      (org-roam-link-auto-replace . nil)
      :config
      (setq org-roam-directory (file-truename "~/Dropbox/roam"))
      (org-roam-mode)
      )

    (leaf org-pomodoro
      :doc "Pomodoro implementation for org-mode."
      :req "alert-0.5.10" "cl-lib-0.5"
      :added "2021-06-02"
      :url "https://github.com/lolownia/org-pomodoro"
      :ensure t
      :after alert
      :hook
      (org-pomodoro-finished-hook
       . (lambda () (start-process "org-pomodoro-finished-notification" nil
				   "dunstify" "--appname" "Emacs.org-pomodoro"
				   "Pomodoro finished! Start break time...")))
      (org-pomodoro-break-finished-hook
       . (lambda () (start-process "org-pomodoro-break-finished-notification" nil
				   "dunstify" "--appname" "Emacs.org-pomodoro"
				   "Pomodoro break is over!")))
      )
  )
  (leaf twittering-mode :ensure t)
  (leaf rainbow-delimiters :ensure t
    :hook
    (prog-mode-hook . rainbow-delimiters-mode))

  (leaf evil :ensure t
    :config
    (evil-mode)
    (leaf evil-org :ensure t)
    (leaf evil-surround :ensure t
      :after 'evil-core
      :config
      (evil-surround-mode)
      (evil-define-key 'visual evil-surround-mode-map "sd" 'evil-surround-delete)
      (evil-define-key 'visual evil-surround-mode-map "sa" 'evil-surround-region)
    )
    (leaf evil-numbers :ensure t
      :after 'evil-core
      :config
      (evil-define-key 'normal 'global
	(kbd "C-c C-a") 'evil-numbers/inc-at-pt
	(kbd "C-c C-x") 'evil-numbers/dec-at-pt
	(kbd "C-c g C-a") 'evil-numbers/inc-at-pt-incremental
	(kbd "C-c g C-x") 'evil-numbers/dec-at-pt-incremental
	)
      )
    )

  (leaf programming
    :config
    (leaf haskell-mode :ensure t)
  (leaf newsticker
    :doc "A Newsticker for Emacs."
    :tag "builtin"
    :added "2021-05-20"
    :hook (newsticker-mode-hook . (lambda () (toggle-truncate-lines -1)))
    :custom
    (newsticker-url-list . '(("Mogura VR" "https://www.moguravr.com/feed" nil nil nil)
			     ("Yahoo top picks" "https://news.yahoo.co.jp/rss/topics/top-picks.xml"
			      nil nil nil)
			     ("Yahoo japan" "https://news.yahoo.co.jp/rss/topics/domestic.xml"
			      nil nil nil)
			     ))
    )
)
  
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
  (setq display-line-numbers-type 'relative)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (auto-revert-mode)

  (show-paren-mode)
  (skk-mode)
)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'init)
