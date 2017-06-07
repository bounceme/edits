;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'line-number-mode) (line-number-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(setq package-list '(emms golden-ratio exec-path-from-shell))

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
(emms-player-mpd-update-all)
(emms-cache-set-from-mpd-all)
(emms-playing-time 1)
; (setq emms-playing-time-style 'bar)
(define-key emms-playlist-mode-map [double-mouse-1] 'emms-playlist-mode-play-smart)

(add-hook 'emms-player-started-hook 'emms-smart-browse)
(add-hook 'emms-player-started-hook 'emms-player-mpd-show)
(emms-mode-line-disable)

(defun emms-clear-play ()
  (interactive)
  (emms-browser-clear-playlist)
  (emms-browser-add-tracks-and-play)
  (emms-playlist-mode-switch-buffer)
  (save-excursion (goto-char (point-min))
                  (emms-walk-tracks
                    (emms-playlist-update-track)))
  (emms-playlist-mode-switch-buffer))

(define-key emms-browser-mode-map [double-mouse-1] 'emms-clear-play)
(define-key emms-browser-mode-map [mouse-1] 'emms-browser-toggle-subitems)

(defun my-info-func (track)
  "Return a description of TRACK."
  (let ((artist (emms-track-get track 'info-artist))
        (title  (emms-track-get track 'info-title))
        (ptime (emms-track-get track 'info-playing-time)))
    (cond
      ((and artist title ptime)
       (concat artist (format " - %-20s - " title) (format "%5s:%.2s" (/ ptime 60)
                                                            (format "%02d" (% ptime 60)))))
      (title
        title)
      (t
        (emms-track-simple-description track)))))
(setq emms-track-description-function 'my-info-func)








(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
