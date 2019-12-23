(setq megra-root (portacle-path "/megra"))
(add-to-list 'load-path (portacle-path "/megra"))
(require 'incudine-megra)

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
	  ("*pi*" . 960) ; pi
          )))

(add-hook 'lisp-mode-hook 'my-pretty-lambda)
(add-hook 'incudine-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(font-lock-add-keywords 'incudine-mode
                        '(("pear" . font-lock-keyword-face)
                          ("pprob" . font-lock-keyword-face)
                          ("skip" . font-lock-keyword-face)
                          ("for" . font-lock-keyword-face)
                          ("prob" . font-lock-keyword-face)
                          ("always" . font-lock-keyword-face)
                          ("pulspread" . font-lock-keyword-face)
                          ("lm" . font-lock-keyword-face)
                          ("lifemodel" . font-lock-keyword-face)
                          ("evr" . font-lock-keyword-face)
                          ("discourage" . font-lock-keyword-face)
                          ("encourage" . font-lock-keyword-face)
                          ("grow" . font-lock-keyword-face)
                          ("grown" . font-lock-keyword-face)
                          ("grow2" . font-lock-keyword-face)
                          ("grown2" . font-lock-keyword-face)
                          ("relax" . font-lock-keyword-face)
                          ("haste" . font-lock-keyword-face)
                          ("cyc" . font-lock-keyword-face)
                          ("cyc2" . font-lock-keyword-face)
                          ("nuc" . font-lock-keyword-face)
                          ("nuc2" . font-lock-keyword-face)
                          ("inh" . font-lock-keyword-face)
                          ("exh" . font-lock-keyword-face)
                          ("inexh" . font-lock-keyword-face)
                          ("slearn" . font-lock-keyword-face)
                          ("sinfer" . font-lock-keyword-face)))

;;(menu-bar-mode -1)

(add-hook 'slime-connected-hook
          (lambda ()	    
            (slime-load-file
	     (concat megra-root "/megra-load.megra"))))

(defun now ()
  (interactive)
  (let ((daily-name (concat (portacle-path "/megra-sketchbook/")
			    (format-time-string "%Y")
			    "/"
			    (format-time-string "%m%d_%H%M"))))
    (find-file (format "/%s.megra" daily-name))))

;; load daily default file ...
(now)

(insert "(in-package :megra)\n\n")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#020202" :foreground "#DFDFDF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "monotype" :family "Noto Mono")))))

(add-to-list 'default-frame-alist '(font . "Noto Mono-14:antialias=subpixel"))
