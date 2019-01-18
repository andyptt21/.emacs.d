;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linking to my wordpress blog and OSC account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org2blog
(use-package org2blog
  :config (setq org2blog/wp-blog-alist
      '(("wordpress"
         :url "https://thescientificshrimper.wordpress.com/xmlrpc.php"
         :username "andyptt21"
         :default-title "The Scientific Shrimper"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil))))

;; Connect to Owens (OSC)
(defun connect-remote ()
  (interactive)
  (dired "/ssh:osu8143@owens.osc.edu:/users/PAS1143/osu8143/"))
