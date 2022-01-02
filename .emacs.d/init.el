(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;; END OF USER CONFIG

;; Set by Emacs itself

;; Custom.el stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "cb49de022f924fee5bee2425a874009b99ce8d7ee76f84227446d037fb4298d3" "27b3336b6115451a340275d842de6e8b1c49ce0bba45210ed640902240f8961d" "6b234feec8db588ad5ec2a9d9d7b935f7a155104b25ccfb94d921c45a2ff7d22" "2ed177de0dfc32a6a32d6109ddfd1782a61bcc23916b7b967fa212666d1aa95c" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(electric-pair-mode lsp-haskell centaur-tabs prettify-symbols prettify-utils pretty-symbols flycheck elpy doom-themes doom-modeline use-package sudo-edit peep-dired general gcmh evil-tutor evil-collection emojify elfeed-goodies dired-open dashboard all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
