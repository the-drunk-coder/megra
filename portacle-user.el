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

(make-face 'font-lock-inner-face)
(set-face-foreground 'font-lock-inner-face "spring green")

(make-face 'font-lock-compose-face)
(set-face-foreground 'font-lock-compose-face "khaki")
(font-lock-add-keywords 'incudine-mode
                        '(("friendship2" . 'font-lock-inner-face)
                          ("friendship" . 'font-lock-inner-face)
                          ("discourage" . font-lock-keyword-face)
                          ("encourage" . font-lock-keyword-face)
                          ("xspread2" . font-lock-keyword-face)
                          ("sharpen" . font-lock-keyword-face)
                          ("flower2" . 'font-lock-inner-face)     
                          ("always" . font-lock-keyword-face)
                          ("flower" . 'font-lock-inner-face)
                          ("fully2" . 'font-lock-inner-face)
                          ("shrink" . font-lock-keyword-face)
                          ("fully" . 'font-lock-inner-face)
                          ("apple" . font-lock-keyword-face)
                          ("grown" . font-lock-keyword-face)                          
                          ("relax" . font-lock-keyword-face)
                          ("haste" . font-lock-keyword-face)                                                    
                          ("inexh" . font-lock-keyword-face)
                          ("learn" . 'font-lock-inner-face)
                          ("infer" . 'font-lock-inner-face)
                          ("oscil" . font-lock-keyword-fadce)
                          ("clear" . font-lock-keyword-face)                          
                          ("ppear" . font-lock-keyword-face)
                          ("chop2" . font-lock-inner-face)
                          ("skip" . font-lock-keyword-face)
                          ("prob" . font-lock-keyword-face)
                          ("life" . font-lock-keyword-face)
                          ("cyc2" . 'font-lock-inner-face)                          
                          ("pear" . font-lock-keyword-face)
                          ("blur" . font-lock-keyword-face)
                          ("xdup" . 'font-lock-compose-face)
                          ("grow" . font-lock-keyword-face)
                          ("nuc2" . 'font-lock-inner-face)                          
                          ("pseq" . font-lock-inner-face)
                          ("tmod" . font-lock-keyword-face)
                          ("fade" . font-lock-keyword-face)
                          ("stop" . font-lock-keyword-face)
                          ("chop" . font-lock-inner-face)                          
                          ("env" . font-lock-keyword-face)
                          ("rew" . font-lock-keyword-face)
                          ("rep" . font-lock-keyword-face)
                          ("rnd" . font-lock-keyword-face)
                          ("evr" . font-lock-keyword-face)
                          ("cmp" . 'font-lock-compose-face)
                          ("cyc" . 'font-lock-inner-face)
                          ("nuc" . 'font-lock-inner-face)
                          ("inh" . font-lock-keyword-face)
                          ("exh" . font-lock-keyword-face)
                          ("sx" . font-lock-keyword-face)))
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
