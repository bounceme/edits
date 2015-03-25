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
(require-package 'ac-html)
(require-package 'linum-relative)
(require-package 'helm)
(require-package 'flycheck)
(require-package 'rainbow-mode)
(require-package 'nyan-mode)
(require-package 'exec-path-from-shell)

 
;; ------------------------------------------------------------------------------
;  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
; (setq exec-path (append exec-path '("/usr/local/bin")))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
 
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

 (add-hook 'html-mode-hook 'ac-html-enable)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)		
; (setq ac-js2-evaluate-calls t)

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

(add-hook 'after-init-hook #'global-flycheck-mode)

; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
; ; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
; ; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
; ; (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.mak$" . web-mode))
; (defun web-mode-hook ()
;   "Config for working with web mode"
;   ;; auto complete and tern
;   (auto-complete-mode 1)
; (setq web-mode-ac-sources-alist
;       '(("css" . (ac-source-words-in-buffer ac-source-css-property ac-source-files-in-current-dir ac-source-filename))
;         ("html" . (ac-source-words-in-buffer ac-source-abbrev ac-source-files-in-current-dir ac-source-filename ac-source-html-attribute-value ac-source-html-tag ac-source-html-attribute))
;         ("php" . (ac-source-words-in-buffer
;                   ac-source-words-in-same-mode-buffers
;                   ac-source-dictionary ac-source-filename))))
; )
; (add-hook 'web-mode-hook 'web-mode-hook)

; (autoload 'js2-mode "js2" nil t)
; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
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


 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (scroll-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
