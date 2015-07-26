 (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
			   '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
(setq my-packages
	  (append
		'(smartparens exec-path-from-shell moe-theme solarized-emacs less-css-mode js-comint evil-nerd-commenter evil-plugins auto-complete yasnippet js2-mode tern expand-region evil evil-leader evil-matchit evil-surround evil-exchange skewer-mode emmet-mode ac-html linum-relative helm rainbow-mode rainbow-delimiters smartparens color-theme-zenburn)
		(mapcar 'el-get-source-name el-get-sources)))
(el-get-cleanup my-packages)
(el-get 'sync my-packages)

; Two items still need installing:evil-smartparens and flycheck

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(add-hook 'after-init-hook #'global-flycheck-mode)

(display-time-mode 1)
;; (setq display-time-default-load-average nil)
;; ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 85 50))

(require 'js-comint)
(setenv "NODE_NO_READLINE" "1")

(setq less-css-lessc-options '("--autoprefix=\"last 2 versions\""))

(setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-selection-value)

;; (defun copy-from-osx ()
;; (shell-command-to-string "pbpaste"))
;; (defun paste-to-osx (text &optional push)
;; (let ((process-connection-type nil))
;; (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;; (process-send-string proc text)
;; (process-send-eof proc))))
;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx) 

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

(evilnc-default-hotkeys)
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
(define-key evil-visual-state-map "gc" 'evilnc-comment-operator)
; (require 'evil-operator-comment)
; (global-evil-operator-comment-mode 1)

; (global-linum-mode t)
; (ac-linum-workaround)
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(show-paren-mode 1)
;  (nyan-mode 1)
(setq ring-bell-function 'ignore)

;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(define-key evil-insert-state-map (kbd "C-j") 'emmet-expand-line)		
(define-key evil-insert-state-map (kbd "C-h") 'emmet-prev-edit-point)		
(define-key evil-insert-state-map (kbd "C-l") 'emmet-next-edit-point)


(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)

(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ex)


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


(require 'helm-config)
(define-key evil-normal-state-map (kbd "C-p") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match    t)
; (global-set-key (kbd "M-y") 'helm-show-kill-ring)

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

;;yasnippet
(yas-global-mode 1)
(setq yas-snippet-dirs
	  '("~/.emacs.d/el-get/snippjs/js-mode/"))


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
;;; use quick help
(setq ac-use-quick-help t
	  ac-quick-help-delay 0.5)
;;; complete in string
(delete 'font-lock-string-face ac-disable-faces)




; (autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
				 '(progn
					(require 'tern-auto-complete)
					(tern-ac-setup)))

; (add-hook 'js2-mode-hook 'ac-js2-mode)
; (setq ac-js2-evaluate-calls t)

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
(add-hook 'html-mode-hook 'ac-html-enable)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'sgml-mode-hook 'emmet-mode)
(setq emmet-move-cursor-between-quotes t)   

(require 'expand-region)
(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "_") 'er/contract-region)

(define-key evil-normal-state-map (kbd "gs") 'transpose-sexps)


;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
(desktop-save-mode 1)

(require 'evil-exchange)
(setq evil-exchange-key (kbd "zx"))
(setq evil-exchange-cancel-key (kbd "zX"))
(evil-exchange-install)
; (require 'evil-snipe)
; (global-evil-snipe-mode 1)
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(global-evil-leader-mode)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil)
(evil-mode 1)

;; (require 'multiple-cursors)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (defvar my-mc-evil-previous-state nil)
;; (defun my-mc-evil-switch-to-insert-state ()
;;   (when (and (bound-and-true-p evil-mode)
;;              (not (memq evil-state '(insert emacs))))
;;     (setq my-mc-evil-previous-state evil-state)
;;     (evil-insert 1)))
;; (defun my-mc-evil-back-to-previous-state ()
;;   (when my-mc-evil-previous-state
;;     (unwind-protect
;;         (case my-mc-evil-previous-state
;;           ((normal visual) (evil-force-normal-state))
;;           (t (message "Don't know how to handle previous state: %S"
;;                       my-mc-evil-previous-state)))
;;       (setq my-mc-evil-previous-state nil))))
;; (add-hook 'multiple-cursors-mode-enabled-hook
;;           'my-mc-evil-switch-to-insert-state)
;; (add-hook 'multiple-cursors-mode-disabled-hook
;;           'my-mc-evil-back-to-previous-state)
;; (defun my-rrm-evil-switch-state ()
;;   (if rectangular-region-mode
;;       (my-mc-evil-switch-to-insert-state)
;;     ;; (my-mc-evil-back-to-previous-state)  ; does not work...
;;     (setq my-mc-evil-previous-state nil)))
;; (add-hook 'rectangular-region-mode-hook 'my-rrm-evil-switch-state)

;; Set default font
(set-face-attribute 'default nil
					:family "Anonymous Pro"
					:height 170
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
	 ["#5f5f5f" "#ff4b4b" "#a1db00" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#ffffff"])
  '(compilation-message-face (quote default))
  '(cua-global-mark-cursor-color "#2aa198")
  '(cua-normal-cursor-color "#839496")
  '(cua-overwrite-cursor-color "#b58900")
  '(cua-read-only-cursor-color "#859900")
  '(custom-enabled-themes (quote (solarized-dark)))
  '(custom-safe-themes
	 (quote
	   ("d7088a7105aa09cc68e3d058f89917e07e0505e0f4ab522a6045ec8092d67c44" "9d0a9ef96b610c7097bde76d077359d464408bc9913bed0318594efdd8811fb6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "acd48beaecc038c0c990e8ac11a4a80e72f6b57a3c43f4b97d8f69ade64ff294" "823ea71cd79048ec98ba0bd131d969fa51ff595f8bdb25640b92e84653d72fb6" "a776135e3d68ebb9c5033799a86290e2243e352f5b8fe6b3b96fbf80c65acd0c" default)))
  '(evil-search-module (quote evil-search))
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
