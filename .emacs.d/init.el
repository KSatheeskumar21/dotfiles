;;; Package -- summary

;;; Commentary:

;;; Code:
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
   '("bd3b9675010d472170c5d540dded5c3d37d83b7c5414462737b60f44351fb3ed" "aca70b555c57572be1b4e4cec57bc0445dcb24920b12fb1fea5f6baa7f2cad02" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "40b961730f8d3c63537d6c3e6601f15c6f6381b9239594c7bf80b7c6a94d3c24" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "cb49de022f924fee5bee2425a874009b99ce8d7ee76f84227446d037fb4298d3" "27b3336b6115451a340275d842de6e8b1c49ce0bba45210ed640902240f8961d" "6b234feec8db588ad5ec2a9d9d7b935f7a155104b25ccfb94d921c45a2ff7d22" "2ed177de0dfc32a6a32d6109ddfd1782a61bcc23916b7b967fa212666d1aa95c" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(doom-modeline-mode t)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(smex projectile yaml-mode electric-pair-mode lsp-haskell prettify-symbols prettify-utils pretty-symbols flycheck elpy doom-themes doom-modeline use-package sudo-edit peep-dired general gcmh evil-tutor evil-collection emojify elfeed-goodies dired-open dashboard all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
