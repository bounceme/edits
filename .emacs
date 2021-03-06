; vim: set foldmethod=marker foldlevel=0:
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'line-number-mode) (line-number-mode -1))
; start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq package-list '(rjsx-mode restclient emms golden-ratio exec-path-from-shell))

(setq package-archives '(("elpa" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

; enable mouse reporting for terminal emulators
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1))))

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(tooltip-mode nil)
(setq show-help-function nil)

(require 'golden-ratio)
(golden-ratio-mode 1)
(require 'cl)
(require 'emms-setup)
(require 'emms-playing-time)

(emms-all)
(emms-default-players)
(emms-cache-enable)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-music-directory "~/Music")
(emms-player-mpd-connect)
(emms-player-mpd-update-all-reset-cache)
(emms-playing-time 1)
(define-key emms-playlist-mode-map [double-mouse-1] 'emms-playlist-mode-play-smart)
(setq emms-browser-alpha-sort-function nil)
(emms-smart-browse)
(add-hook 'emms-player-started-hook 'emms-show)
(emms-mode-line-disable)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                "   " mode-line-position "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(defun form-track (&optional arg)
  (save-excursion (emms-playlist-mode-switch-buffer)
                  (goto-char (point-min))
                  (emms-walk-tracks
                    (emms-playlist-update-track))
                  (emms-playlist-mode-switch-buffer)))

(defun emms-clear-play ()
  (interactive)
  (emms-browser-clear-playlist)
  (emms-browser-add-tracks-and-play))

(defun emms-append-correctly ()
  (interactive)
  (let ((eline  (save-excursion (save-current-buffer (emms-playlist-mode-switch-buffer)
                                                     (point-max)))))
    (emms-browser-add-tracks)
    (save-excursion (emms-playlist-mode-switch-buffer)
                    (goto-char eline)
                    (emms-walk-tracks
                      (emms-player-mpd-add (emms-playlist-track-at) #'ignore #'ignore))
                    (emms-playlist-mode-switch-buffer))))

(add-hook 'emms-browser-tracks-added-hook 'form-track)


(define-key emms-browser-mode-map (kbd "D")
            (lambda () (interactive)
              (let ((current-prefix-arg '(4)))
                (call-interactively 'emms-browser-delete-files))))
(define-key emms-browser-mode-map (kbd "RET") 'emms-append-correctly)
(define-key emms-browser-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "<right>") 'emms-next)
(define-key emms-playlist-mode-map (kbd "<left>") 'emms-previous)

(define-key emms-browser-mode-map [double-mouse-1] 'emms-clear-play)
(define-key emms-browser-mode-map [double-mouse-1] 'emms-clear-play)
(define-key emms-browser-mode-map [mouse-1] (lambda ()
                                              (interactive)
                                              (when (not (eq (emms-browser-level-at-point) 3))
                                                (emms-browser-toggle-subitems))))

(defun my-info-func (track)
  "Return a description of TRACK."
  (let ((artist (emms-track-get track 'info-artist))
        (title  (emms-track-get track 'info-title))
        (ptime (emms-track-get track 'info-playing-time)))
    (cond
      ((and artist title ptime)
       (concat artist (format " - %-20s - " title) (format "%5s:%.2s" (/ ptime 60)
                                                           (concat (format "%02d" (% ptime 60)) "0"))))
      (title
        title)
      (t
        (emms-track-simple-description track)))))
(setq emms-track-description-function 'my-info-func)

; modeline-buttons {{{1
(defun mpdseek-button (sign)
  "seek forward or backwards 1/10 of song length"
  (emms-player-seek (/ (emms-track-get (emms-playlist-current-selected-track) 'info-playing-time)
                       (* sign 10))))

(defconst my-mode-line-map4 (make-sparse-keymap))
(setq global-mode-string
      (append (list
                (propertize " ⏭ "
                            'local-map my-mode-line-map4
                            'mouse-face 'mode-line-highlight))
              global-mode-string))
(define-key my-mode-line-map4
            [mode-line mouse-1] (lambda ()
                                  (interactive)
                                  (emms-next)))

(defconst my-mode-line-map2 (make-sparse-keymap))
(setq global-mode-string
      (append (list
                (propertize " ⏩ "
                            'local-map my-mode-line-map2
                            'mouse-face 'mode-line-highlight))
              global-mode-string))
(define-key my-mode-line-map2
            [mode-line mouse-1]  (lambda ()
                                   (interactive)
                                   (mpdseek-button 1)))

; play/pause
(defconst my-mode-line-map (make-sparse-keymap))
(setq global-mode-string
      (append (list
                (propertize " ⏯ "
                            'local-map my-mode-line-map
                            'mouse-face 'mode-line-highlight))
              global-mode-string))
(define-key my-mode-line-map
            [mode-line mouse-1] (lambda ()
                                  (interactive)
                                  (emms-pause)))

(defconst my-mode-line-map3 (make-sparse-keymap))
(setq global-mode-string
      (append (list
                (propertize " ⏪ "
                            'local-map my-mode-line-map3
                            'mouse-face 'mode-line-highlight))
              global-mode-string))
(define-key my-mode-line-map3
            [mode-line mouse-1] (lambda ()
                                  (interactive)
                                  (mpdseek-button -1)))

(defconst my-mode-line-map1 (make-sparse-keymap))
(setq global-mode-string
      (append (list
                (propertize " ⏮ "
                            'local-map my-mode-line-map1
                            'mouse-face 'mode-line-highlight))
              global-mode-string))
(define-key my-mode-line-map1
            [mode-line mouse-1] (lambda ()
                                  (interactive)
                                  (emms-previous)))

;}}}

(put 'emms-browser-delete-files 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (anti-zenburn)))
 '(custom-safe-themes
   (quote
    ("bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" default)))
 '(package-selected-packages
   (quote
    (niflheim-theme anti-zenburn-theme rjsx-mode restclient golden-ratio exec-path-from-shell emms))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
