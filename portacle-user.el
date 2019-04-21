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

(menu-bar-mode -1)

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
