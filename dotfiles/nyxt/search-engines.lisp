(in-package #:nyxt-user)

(nx-search-engines:define-search-engine archwiki
    (:shortcut "archw"
     :fallback-url (quri:uri "https://wiki.archlinux.jp/index.php")
     :base-search-url "https://wiki.archlinux.jp/index.php?search=~a"
     :documentation "Arch wiki search."))

(nx-search-engines:define-search-engine hoogle
    (:shortcut "hoogle"
     :fallback-url (quri:uri "https://hoogle.haskell.org")
     :base-search-url "https://hoogle.haskell.org/?hoogle=~a"
     :documentation "Hoogle, Haskell API search engine"))
(nx-search-engines:define-search-engine hackage
    (:shortcut "hackage"
     :fallback-url "https://hackage.haskell.org/packages/"
     :base-search-url "https://hackage.haskell.org/packages/search?terms={}"
     :documentation "The Haskell Package Repository"))

(define-configuration (buffer web-buffer)
  ((search-engines (list (engines:google :shortcut "g"
                                         :safe-search nil)
			 (engines:github :shortcut "github")
			 (engines:arch :shortcut "package")
			 (archwiki :shortcut "archw")
			 (hoogle)
			 (hackage)
                         (engines:duckduckgo :theme :terminal
                                             :help-improve-duckduckgo nil
                                             :homepage-privacy-tips nil
                                             :privacy-newsletter nil
                                             :newsletter-reminders nil
                                             :install-reminders nil
                                             :install-duckduckgo nil)))))
