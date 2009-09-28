;;; slackware-changelog.el --- improvment view slackware ChangeLog

;; Copyright (C) 2009

;; Author:  <owner.mad.epa@gmail.com>
;; Version: 0.1
;; Keywords: slackware

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package work only (?) local mirrors.

;; If your have get local mirror slackware repo, run this string:

;; rsync -P --delete -avzlhH  mirror.yandex.ru::slackware/slackware-current /pub/mirrors/

;; where /pub/mirrors is yout local dir and add here string to crontab

;;; Installing:

;; add to your init file this string:

;; (add-to-list 'load-path "/path/to/slackware-changelog/")
;; (require 'slackware-changelog)
;; (setq slackware-mirror-root "/path/to/mirror/slckware/")

;;; BUG:

;; - if package begining xf86-video-* or xorg-server-* (maybe other)
;;   then first package (witch match xf86-video-VERSION-ARCH-BUILD)
;;   marked conflict

;;; TODO:

;; - install/remove/update from emacs
;; - work with remote repo
;; - show info about other package (not official repo)
;; - check broken version (e.g. r994599)

;;; Code:

(defgroup slackware-faces nil
  "Faces for displaying ChangeLog.txt"
  :group 'slackware)

(defvar slackware-mirror-root nil
  "Path to your mirror slackware repo
\(e.g. /pub/mirrors/slackware-current/\)")

(defvar slackware-buffer "*slackware-changelog*")

(defvar slackware-pkg-regex "\\(.*-[0-9]+\\.\\(txz\\|tgz\\)\\):")

(defvar slackware-changelog-field 10
  "Print last ten field of ChangeLog")

(defface slackware-face-pkg-normal
  '((t (:weight bold :foreground "blue")))
  "face for displaying normal package"
  :group 'slackware-faces)

(defface slackware-face-pkg-conflict
  '((t (:background "red" :foreground "black")))
  "face for displaying conflict package"
  :group 'slackware-faces)

(defface slackware-face-pkg-version-less
  '((t (:slant italic :foreground "red4")))
  "face for displaying less version package"
  :group 'slackware-faces)

(defface slackware-face-pkg-not-exist
  '((t (:slant italic :foreground "gray50")))
  "face for displaying not exist package"
  :group 'slackware-faces)

(defface slackware-face-upgraded
  '((t (:foreground "green4" :weight bold)))
  "face for displaying upgraded package"
  :group 'slackware-faces)

(defface slackware-face-rebuilt
  '((t (:foreground "green4")))
  "face for displaying rebuilt package"
  :group 'slackware-faces)

(defface slackware-face-added
  '((t (:foreground "green4" :weight bold)))
  "face for displaying added package"
  :group 'slackware-faces)

(defface slackware-face-removed
  '((t (:foreground "red")))
  "face for displaying removed package"
  :group 'slackware-faces)

(defface slackware-face-fixed
  '((t (:slant italic :weight bold)))
  "face for displaying fixed package"
  :group 'slackware-faces)

(defface slackware-face-patched
  '((t (:weight bold :foreground "yellow4")))
  "face for displaying pathced package"
  :group 'slackware-faces)

(defface slackware-face-security
  '((t (:weight bold :foreground "red4")))
  "face for displaying security fix"
  :group 'slackware-faces)

(defun slackware-mode ()
  "Slackware ChangeLog mode"
  (interactive)
  (switch-to-buffer slackware-buffer)
  (use-local-map slackware-mode-map)
  (delete-region (point-min) (point-max))
  (save-excursion
    (insert-file (concat slackware-mirror-root "/ChangeLog.txt"))
    (goto-char (point-min))
    (delete-region (or (search-forward "+--------------------------+"
                                       nil t slackware-changelog-field)
                       (point-max))
                   (point-max)))
  (slackware-markup)
  (toggle-read-only)
  (slackware-mode))

(defun slackware-markup ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward slackware-pkg-regex nil t)
      (set-text-properties (match-beginning 1) (match-end 1)
                           (slackware-pkg-check (match-string-no-properties 1))))
    (goto-char (point-min))
    (while (re-search-forward "\\(Rebuilt\\)\\|\\(Upgraded\\)\\|\\(Added\\)\\|\\(Removed\\)\\|\\(Fixed\\)\\|\\(Patched\\)" nil t)
      (cond
       ((match-string 1) ;; Rebuild
        (set-text-properties (match-beginning 1) (match-end 1)
                             `(face slackware-face-rebuilt)))
       ((match-string 2) ;; Upgraded
        (set-text-properties (match-beginning 2) (match-end 2)
                             `(face slackware-face-upgraded)))
       ((match-string 3) ;; Added
        (set-text-properties (match-beginning 3) (match-end 3)
                             `(face slackware-face-added)))
       ((match-string 4) ;; Removed
        (set-text-properties (match-beginning 4) (match-end 4)
                             `(face slackware-face-removed)))
       ((match-string 5) ;; Fixed
        (set-text-properties (match-beginning 5) (match-end 5)
                             `(face slackware-face-fixed)))
       ((match-string 6) ;; Patched
        (set-text-properties (match-beginning 6) (match-end 6)
                             `(face slackware-face-patched)))))
    (goto-char (point-min))
    (while (re-search-forward "\(\\* Security fix \\*\)" nil t)
      (set-text-properties (match-beginning 0) (match-end 0)
                           `(face slackware-face-security)))))

(defun slackware-changelog-update ()
  (interactive)
  (toggle-read-only nil)
  (slackware-markup)
  (toggle-read-only))

(defvar slackware-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "i" 'slackware-install)
    (define-key map "r" 'slackware-remove)
    (define-key map "u" 'slackware-upgrade)
    (define-key map "g" 'slackware-changelog-update)
    (define-key map "h" 'slackware-info)
    (define-key map "n" 'slackware-next-pkg)
    (define-key map "p" 'slackware-previous-pkg)
    (define-key map "q" 'slackware-quit)
    map)
  "Keymap for `slackware-mode'.")

(defun slackware-info ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward slackware-pkg-regex nil t)
        (let* ((pkg (match-string-no-properties 1))
               (pkg (replace-regexp-in-string "\\(tgz\\|txz\\)" "txt" pkg)))
          (message (concat "Getting info " slackware-mirror-root
                           (if (string-match "extra/" pkg)
                               pkg
                             (concat "slackware/" pkg)))))
      (message "Package not found"))))

(defun slackware-install ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward slackware-pkg-regex nil t)
        (let* ((pkg (match-string-no-properties 1))
               (pkg (if (string-match "extra/" pkg)
                               pkg
                      (concat "slackware/" pkg)))
               (pkg (concat slackware-mirror-root pkg)))
          (kill-new (concat "sudo installpkg " pkg))
          (message "Install command save to kill-ring"))
      (message "Package not found"))))

(defun slackware-remove ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward slackware-pkg-regex nil t)
        (let* ((pkg (match-string-no-properties 1))
               (pkg (if (string-match "extra/" pkg)
                               pkg
                      (concat "slackware/" pkg)))
               (pkg (concat slackware-mirror-root pkg)))
          (kill-new (concat "sudo removepkg " pkg))
          (message "Remove command save to kill-ring"))
      (message "Package not found"))))

(defun slackware-upgrade ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
        (if (re-search-forward slackware-pkg-regex nil t)
        (let* ((pkg (match-string-no-properties 1))
               (pkg (if (string-match "extra/" pkg)
                               pkg
                      (concat "slackware/" pkg)))
               (pkg (concat slackware-mirror-root pkg)))
          (kill-new (concat "sudo upgradepkg " pkg))
          (message "Upgrade command save to kill-ring"))
      (message "Package not found"))))

(defun slackware-pkg-check (pkg-name)
  (let* ((pkg-var (progn
                    ;; PATH/TO/PKG_NAME-VERSION-ARCH-BUILD.prefix
                    (string-match "\\(.*/\\)?\\(\\(.*\\)-\\(.*\\)-.*-.*\\)\\..*" pkg-name)
                    (list (match-string 2 pkg-name)    ;; pkg-name with prefix
                          (match-string 3 pkg-name)    ;; real pkg-name
                          (match-string 4 pkg-name)))) ;; pkg version
         (pkg-name (nth 0 pkg-var))
         (pkg-real-name (nth 1 pkg-var))
         (pkg-version (nth 2 pkg-var))
         ;; stripping _smp or _git version
         (pkg-version (if (string-match-p "_" pkg-version)
                          (substring pkg-version 0
                                     (string-match "_" pkg-version))
                        pkg-version)))
    (if (file-exists-p (concat "/var/log/packages/" pkg-name))
        `(face slackware-face-pkg-normal)
      (let* ((maybe-pkg (file-expand-wildcards
                         (concat "/var/log/packages/" pkg-real-name "*")))
             (maybe-pkg-conflict (if (> (length maybe-pkg) 1) t nil))
             (maybe-pkg-exist (if (= (length maybe-pkg) 1) t nil))
             (maybe-pkg (if maybe-pkg-exist (car maybe-pkg)))
             (maybe-pkg-var
              (when maybe-pkg-exist
                ;; PATH/TO/PKG_NAME-VERSION-ARCH-BUILD
                (string-match "\\(.*\\)?/\\(.*\\)-\\(.*\\)-.*-.*$"  maybe-pkg)
                (list (match-string 2 maybe-pkg) (match-string 3 maybe-pkg))))
             (maybe-pkg-name (if maybe-pkg-exist (nth 0 maybe-pkg-var)))
             (maybe-pkg-version (if maybe-pkg-exist (nth 1 maybe-pkg-var)))
             ;; stripping _smp or _git version
             (maybe-pkg-version (if maybe-pkg-exist
                                    (substring maybe-pkg-version 0
                                               (or (string-match "_" maybe-pkg-version)
                                                   (length maybe-pkg-version)))
                                  maybe-pkg-version)))
        (cond
         (maybe-pkg-conflict
          `(face slackware-face-pkg-conflict))
         ((not maybe-pkg-exist)
          `(face slackware-face-pkg-not-exist))
         ;; FIXME: found broken version (e.g. r994599)
         ((condition-case nil
              (if (version< maybe-pkg-version pkg-version)
                  `(face slackware-face-pkg-version-less))
            (error nil)))
         (t
          `(face slackware-face-pkg-normal)))))))

(defun slackware-next-pkg ()
  (interactive)
  (move-end-of-line 1)
  (re-search-forward slackware-pkg-regex nil t)
  (move-beginning-of-line 1))

(defun slackware-previous-pkg ()
  (interactive)
  (move-beginning-of-line 1)
  (re-search-backward slackware-pkg-regex nil t)
  (move-beginning-of-line 1))

(defun slackware-quit ()
  (interactive)
  (kill-buffer slackware-buffer))

(provide 'slackware-changelog)
;;; slackware-changelog.el ends here
