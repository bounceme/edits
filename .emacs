(require 'package)
 
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source))
 
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
 
(defun require-package (p)
  (unless (package-installed-p p)
    (package-install p))
  (require p))
 
(require-package 'auto-complete)
(require-package 'yasnippet)
(require-package 'js2-mode)
(require-package 'ac-js2)
(require-package 'expand-region)
(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-matchit)
(require-package 'evil-snipe)
(require-package 'evil-surround)
(require-package 'evil-exchange)
(require-package 'skewer-mode)
(require-package 'emmet-mode)
; (require-package 'web-mode)
(require-package 'ac-html)
(require-package 'linum-relative)
(require-package 'helm)
(require-package 'flycheck)

 
;; ------------------------------------------------------------------------------
 
 
 
(global-linum-mode t)
(show-paren-mode 1)
 (nyan-mode 1)
(setq ring-bell-function 'ignore)
 
;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(define-key evil-insert-state-map (kbd "C-j") 'emmet-expand-line)
(define-key evil-insert-state-map (kbd "C-h") 'emmet-prev-edit-point)
(define-key evil-insert-state-map (kbd "C-l") 'emmet-next-edit-point)


(setq-default cursor-type `(box))
(setq evil-normal-state-cursor `(box))
(setq evil-insert-state-cursor `(box))
(setq evil-visual-state-cursor `(box))
(setq evil-operator-state-cursor `(box))
(setq evil-replace-state-cursor `(box))
(setq evil-motion-state-cursor `(box))
(require 'linum-relative)
(setq linum-relative-current-symbol "")

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "p" 'helm-show-kill-ring)


(require 'helm-config)
(define-key evil-normal-state-map (kbd "C-p") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
; (global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;yasnippet
(yas-global-mode 1)
 
;; auto-complete
; (define-globalized-minor-mode real-global-auto-complete-mode
;   auto-complete-mode (lambda ()
;                        (if (not (minibufferp (current-buffer)))
;                          (auto-complete-mode 1))
;                        ))
; (real-global-auto-complete-mode t)
; (ac-set-trigger-key "TAB")
; (ac-set-trigger-key "<tab>")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-ignore-case nil)
;;; use quick help
(setq ac-use-quick-help t
      ac-quick-help-delay 0.5)
;;; complete in string
(delete 'font-lock-string-face ac-disable-faces)

; (require-package 'flycheck)
; (global-flycheck-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)
; (setq flycheck-check-syntax-automatically '(mode-enabled save new-line idle-change))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . web-mode))
(defun web-mode-hook ()
  "Config for working with web mode"
  ;; auto complete and tern
  (auto-complete-mode 1)
; (setq web-mode-ac-sources-alist
;       '(("css" . (ac-source-words-in-buffer ac-source-css-property ac-source-files-in-current-dir ac-source-filename))
;         ("html" . (ac-source-words-in-buffer ac-source-abbrev ac-source-files-in-current-dir ac-source-filename ac-source-html-attribute-value ac-source-html-tag ac-source-html-attribute))
;         ("php" . (ac-source-words-in-buffer
;                   ac-source-words-in-same-mode-buffers
;                   ac-source-dictionary ac-source-filename))))
)
(add-hook 'web-mode-hook 'web-mode-hook)

; (autoload 'js2-mode "js2" nil t)
; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(setq emmet-move-cursor-between-quotes t)   

(require 'expand-region)
(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "_") 'er/contract-region)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
(desktop-save-mode 1)

(require 'evil-exchange)
;; change default key bindings (if you want) HERE
(setq evil-exchange-key (kbd "zx"))
(setq evil-exchange-cancel-key (kbd "zX"))
(evil-exchange-install)
(require 'evil-snipe)
(global-evil-snipe-mode 1)
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(global-evil-leader-mode)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil)
  (evil-mode 1)

;; Set default font
(set-face-attribute 'default nil
                    :family "Anonymous Pro"
                    :height 150
                    :weight 'normal
                    :width 'normal)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (scroll-bar-mode -1)
