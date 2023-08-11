(setq skk-get-jisyo-directory "/usr/local/share/skk")
(setq skk-sticky-key nil)
(add-to-list 'auto-mode-alist '(".*\\.dict$" . skk-jisyo-edit-mode))
(setq skk-jisyo-code 'utf-8)
(setq skk-large-jisyo "/usr/local/share/skk/SKK-JISYO.L")
(setq skk-jisyo "/usr/local/share/skk/personal.dict")

(setq skk-use-azik t)
(setq skk-rom-kana-rule-list
      (seq-remove (lambda (c) (eq ?+ c))
		  (append '(("wi" nil "ヰ"))
			  skk-rom-kana-rule-list)))

(setq skk-azik-keyboard-type 'us101)
