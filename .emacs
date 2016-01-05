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
		      evil-indent-textobject
		      evil-matchit
		      evil-nerd-commenter
		      evil-leader
		      evil-snipe
		      monokai-theme
		      ;; linum-relative
		      smartparens
		      evil-smartparens
		      less-css-mode
		      js-comint
		      company
		      company-tern
		      yasnippet
		      js2-mode
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

;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-selection-value)

;; (fset 'evil-visual-update-x-selection 'ignore)
					; (enable 'evil-visual-update-x-selection 'ignore)

;; (defun x-select-text (text))
;; (setq x-select-enable-clipboard nil)
;; (setq x-select-enable-primary nil)
;; (setq mouse-drag-copy-region nil)

;; (define-key evil-visual-state-map "p" (lambda () (interactive) (evil-paste-from-register ?\")))

(setq interprogram-cut-function 'simpleclip-set-contents)
(setq interprogram-paste-function 'simpleclip-get-contents) 

(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;; (global-linum-mode 1)
;; (require 'linum-relative)
;; (setq linum-relative-current-symbol "")
;; (linum-relative-on)

(global-relative-line-numbers-mode)
(setq relative-line-numbers-format #'abs-rel-numbers)
(defun abs-rel-numbers (offset)
  (if (= offset 0)
      ;; current line
      (format "%4d " (line-number-at-pos))
    ;; not the current line
    (format "%4d " (abs offset))
    ))

(show-paren-mode 1)
(setq ring-bell-function 'ignore)

;; (helm-mode 1)
(require 'helm-config)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match    t)
(define-key evil-normal-state-map (kbd "C-p") 'helm-mini)
(setq helm-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s")))
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)

(evilnc-default-hotkeys)
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
(define-key evil-visual-state-map "gc" 'evilnc-comment-operator)

;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(define-key evil-insert-state-map (kbd "C-j") 'emmet-expand-line)		
(define-key evil-insert-state-map (kbd "C-h") 'emmet-prev-edit-point)		
(define-key evil-insert-state-map (kbd "C-l") 'emmet-next-edit-point)

(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd "-") 'dired-up-directory) ))


(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ex)

(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)


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
  "P" 'evil-paste-pop-next
  "m" 'compile-less-css
  "c" 'helm-M-x)

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

(require 'smartparens-config)
(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)
(add-hook 'js2-mode #'yas-minor-mode)

					; (autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq-default js2-mode-show-parse-errors nil
	      js2-mode-show-strict-warnings nil)
(autoload 'flycheck-get-checker-for-buffer "flycheck")
(defun sanityinc/disable-js2-checks-if-flycheck-active ()
  (unless (flycheck-get-checker-for-buffer)
    (set (make-local-variable 'js2-mode-show-parse-errors) t)
    (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
(add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
(add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
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

;; (defun skewer-eval-region (beg end)
;;   "Execute the region as JavaScript code in the attached browsers."
;;   (interactive "r")
;;   (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun skewer-eval-region (start end)
  "Evaluate the region as JavaScript code."
  (interactive "r")
  (skewer-eval (buffer-substring-no-properties start end)
	       #'skewer-post-minibuffer))

(add-hook 'js2-mode-hook 'skewer-mode)
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
(desktop-save-mode 1)
(add-to-list 'desktop-locals-to-save 'buffer-undo-list)
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
      evil-snipe-enable-highlight t
      evil-snipe-enable-incremental-highlight t
      evil-snipe-smart-case t)
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(global-evil-leader-mode)
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Set default font
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 140
		    :weight 'normal
		    :width 'normal)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (warm-night)))
 '(custom-safe-themes
   (quote
    ("ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" default)))
 '(evil-want-Y-yank-to-eol t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
