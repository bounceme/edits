;; Turn off mouse interface early in startup to avoid momentary display
;;; Code:
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(exec-path-from-shell
		      evil
		      simpleclip
		      evil-indent-textobject
		      evil-matchit
		      evil-nerd-commenter
		      evil-leader
		      evil-snipe
		      monokai-theme
		      ;; linum-relative
		      autopair
		      evil-magit
		      less-css-mode
		      js-comint
		      company
		      company-tern
		      yasnippet
		      js2-mode
		      js3-mode
		      expand-region
		      evil-surround
		      evil-exchange
		      skewer-mode
		      emmet-mode
		      rainbow-mode
		      rainbow-delimiters
		      wgrep
		      highlight-numbers
		      helm
		      warm-night-theme
		      magit
		      flycheck
		      solarized-theme))
(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'evil)
(evil-mode 1)
(evil-mode)

;; (defun my-evil-unimpaired-insert-newline-above (count)
;;   "Insert an empty line below point"
;;   (interactive "p")
;;   (save-excursion
;;     (dotimes (i count)
;;       (evil-insert-newline-above))))
;; (defun my-evil-unimpaired-insert-newline-below (count)
;;   "Insert an empty line below point"
;;   (interactive "p")
;;   (save-excursion
;;     (dotimes (i count)
;;       (evil-insert-newline-below))))
;; (with-eval-after-load 'evil-maps
;;   (define-key evil-normal-state-map (kbd "[ SPC") 'my-evil-unimpaired-insert-newline-above)
;;   (define-key evil-normal-state-map (kbd "] SPC") 'my-evil-unimpaired-insert-newline-below))

(require 'evil-magit)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

(display-time-mode 1)
;; (setq display-time-default-load-average nil)
;; ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))

(require 'js-comint)
(setenv "NODE_NO_READLINE" "1")

(setq less-css-lessc-options '("--autoprefix=\"last 2 versions\""))

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;; emacs is evil and decrees that horizontal shall henceforth be vertical
(setq-default ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-merge-split-window-function 'split-window-horizontally)

(require 'simpleclip)
(fset 'evil-visual-update-x-selection 'ignore)
(setq save-interprogram-paste-before-kill t)

;; (fset 'evil-visual-update-x-selection 'ignore)

;; ;; (defun x-select-text (text))
;; (setq x-select-enable-clipboard nil)
;; (setq x-select-enable-primary nil)
;; (setq mouse-drag-copy-region nil)

;; (define-key evil-visual-state-map "p" (lambda () (interactive) (evil-paste-from-register ?\")))

;; (setq interprogram-cut-function 'simpleclip-set-contents)
;; (setq interprogram-paste-function 'simpleclip-get-contents) 

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;; (global-linum-mode 1)
;; (require 'linum-relative)
;; (setq linum-relative-current-symbol "")
;; (linum-relative-on)

;; (global-relative-line-numbers-mode)
;; (setq relative-line-numbers-format #'abs-rel-numbers)
;; (defun abs-rel-numbers (offset)
;;   (if (= offset 0)
;;       ;; current line
;;       (format "%4d " (line-number-at-pos))
;;     ;; not the current line
;;     (format "%4d " (abs offset))
;;     ))

(show-paren-mode 1)
(setq ring-bell-function 'ignore)

(helm-mode 1)
(require 'helm-config)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match    t)
(define-key evil-normal-state-map (kbd "C-p") 'helm-for-files)

(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)

(evilnc-default-hotkeys)
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
(define-key evil-visual-state-map "gc" 'evilnc-comment-operator)

;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; (evil-define-key 'normal js2-mode (kbd "K")
;;   'tern-get-docs)
;; (evil-define-key 'normal js2-mode (kbd "C-]")
;;   'tern-find-definition)
;; ( define-key evil-normal-state-local-map  (kbd "C-]") 'tern-find-definition )
(add-hook 'js3-mode-hook
	  '(lambda ()
	     ( define-key evil-normal-state-local-map  (kbd "C-]") 'tern-find-definition )
	     ( define-key evil-normal-state-local-map  (kbd "K") 'tern-get-docs )
	     ))

(define-key evil-insert-state-map (kbd "C-j") 'emmet-expand-line)		
(define-key evil-insert-state-map (kbd "C-h") 'emmet-prev-edit-point)		
(define-key evil-insert-state-map (kbd "C-l") 'emmet-next-edit-point)

(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd "-") 'dired-up-directory) ))


(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ex)

(define-key evil-normal-state-map (kbd "zl") 'evil-window-right)
(define-key evil-normal-state-map (kbd "zh") 'evil-window-left)
(define-key evil-normal-state-map (kbd "zj") 'evil-window-down)
(define-key evil-normal-state-map (kbd "zk") 'evil-window-up)

(define-key evil-normal-state-map (kbd "[ SPC") (lambda() (interactive)(evil-insert-newline-above) (forward-line)))
(define-key evil-normal-state-map (kbd "] SPC") (lambda() (interactive)(evil-insert-newline-below) (forward-line -1)))

(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)

(define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
(define-key evil-visual-state-map (kbd "] e") ":move'>+1")

(defun unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))
(defun unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))
(define-key evil-normal-state-map (kbd "[ p") 'unimpaired/paste-above)
(define-key evil-normal-state-map (kbd "] p") 'unimpaired/paste-below)

;; (global-set-key (kbd "C-l") 'evil-window-right)
;; (global-set-key (kbd "C-h") 'evil-window-left)
;; (global-set-key (kbd "C-j") 'evil-window-down)
;; (global-set-key (kbd "C-k") 'evil-window-up)


(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(setq-default cursor-type `(box))
(setq evil-normal-state-cursor `(box))
(setq evil-insert-state-cursor `(box))
(setq evil-visual-state-cursor `(box))
(setq evil-operator-state-cursor `(box))
(setq evil-replace-state-cursor `(box))
(setq evil-motion-state-cursor `(box))

					; (define-key global-map (kbd "RET") 'newline-and-indent)

(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "f" 'helm-find
  "p" 'evil-paste-pop
  "c" 'simpleclip-copy
  "v" 'simpleclip-paste
  "x" 'simpleclip-cut
  "P" 'evil-paste-pop-next
  "m" 'compile-less-css
  )

(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
	(outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
	 (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
	 (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
;; create "il"/"al" (inside/around) line text objects:
(define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
;; create "ie"/"ae" (inside/around) entire buffer text objects:
(define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'")

(require 'autopair)
(autopair-global-mode)

;; (require 'smartparens-config)
;; (smartparens-global-mode 1)
;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
;; (sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
;; (defun my-create-newline-and-enter-sexp (&rest _ignored)
;;   "Open a new brace or bracket expression, with relevant newlines and indent. "
;;   (newline)
;;   (indent-according-to-mode)
;;   (forward-line -1)
;;   (indent-according-to-mode))

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(add-hook 'js2-mode #'yas-minor-mode)

					(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))

(setq-default js3-mode-show-parse-errors nil
	      js3-mode-show-strict-warnings nil)
(autoload 'flycheck-get-checker-for-buffer "flycheck")
(defun sanityinc/disable-js2-checks-if-flycheck-active ()
  (unless (flycheck-get-checker-for-buffer)
    (set (make-local-variable 'js3-mode-show-parse-errors) t)
    (set (make-local-variable 'js3-mode-show-strict-warnings) t)))
(add-hook 'js3-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

(add-hook 'js3-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
				 '(progn
					(require 'tern-auto-complete)
					(tern-ac-setup)))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
     (define-key company-active-map [tab] 'company-complete-common-or-cycle)))
(setq company-selection t)
(setq company-idle-delay 0)

(defun skewer-eval-region (start end)
  "Evaluate the region as JavaScript code."
  (interactive "r")
  (skewer-eval (buffer-substring-no-properties start end)
	       #'skewer-post-minibuffer))

(add-hook 'js3-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)
(add-to-list 'auto-mode-alist '("\.less$" . less-css-mode))
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
(require 'undohist)
(undohist-initialize)

(require 'evil-exchange)
(setq evil-exchange-key (kbd "zx"))
(setq evil-exchange-cancel-key (kbd "zX"))
(evil-exchange-install)
(require 'evil-snipe)
(evil-snipe-mode 1)
(setq evil-snipe-scope 'buffer
      evil-snipe-repeat-scope 'buffer
      evil-snipe-repeat-keys 'nil
      evil-snipe-enable-highlight t
      evil-snipe-enable-incremental-highlight t
      evil-snipe-smart-case t)
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(global-evil-leader-mode)
(require 'evil-surround)
(global-evil-surround-mode 1)
(set-frame-font "Monaco 14")
;; Set default font
; (set-face-attribute 'default nil
; 		    :family "Monaco"
; 		    :height 140
; 		    :weight 'normal
; 		    :width 'normal)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (badwolf)))
 '(custom-safe-themes
   (quote
    ("c95a117efd4eeefc410aa55ea883f74f07ffd64a887538e629a6c7f845fa6add" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "96ed8a4273836a97fcf73811ae4edf32cfc63cd61bbcf64a2cf4305a5aa1953f" "636290b2641b6c6073ad9e5f3402c011fd846e972290758fc36139fb29066f38" "76a0fbbceefb800c78b38fcb9700567248d6514a2dbdb47b7c0578b27d4340f5" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "98a619757483dc6614c266107ab6b19d315f93267e535ec89b7af3d62fb83cad" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" default)))
 '(evil-search-module (quote evil-search))
 '(evil-snipe-repeat-keys nil)
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#3E3D31")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(js-flat-functions t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(js3-boring-indentation t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-continued-expr-mult 1)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "color-23")
 '(pos-tip-foreground-color "color-230")
 '(read-buffer-completion-ignore-case t)
 '(scalable-fonts-allowed t)
 '(simpleclip-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
