;;; slackware-changelog.el --- improvment view slackware ChangeLog

;; Copyright (C) 2009

;; Author:  <owner.mad.epa@gmail.com>
;; Version: 0.1
;; Keywords: slackware, pkg

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

;;

;;; TODO:

;; + simple highlight
;; + keystroke to add/remove/upgrade (use pkgtools)
;; - realy install

;;; Code:

(defgroup slackware-faces nil
  "Faces for displaying ChangeLog.txt"
  :group 'slackware)

(defvar slackware-mirror-root nil
  "Path to your mirror slackware repo
\(e.g. /pub/mirrors/slackware-current/\)")

(defvar slackware-buffer " *slackware-changelog*")

(defvar slackware-pkg-regex "\\(.*-[0-9]+\\.\\(txz\\|tgz\\)\\):")

(defface slackware-face-archive
  '((t (:weight bold :foreground "blue")))
  "face for displaying archive package"
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

(defun slackware-changelog ()
  "slackware changelog mode"
  (interactive)
  (switch-to-buffer slackware-buffer)
  (delete-region (point-min) (point-max))
  (insert-file (concat slackware-mirror-root "/ChangeLog.txt"))
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward slackware-pkg-regex nil t)
      (set-text-properties (match-beginning 1) (match-end 1)
                           `(face slackware-face-archive)))
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
                           `(face slackware-face-security))))
  (setq buffer-read-only t)
  (slackware-mode))

(define-derived-mode slackware-mode text-mode
  "Slackware ChangeLog mode"
  "Major mode for Slackware ChangeLog mode")

(defvar slackware-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "i" 'slackware-install)
    (define-key map "r" 'slackware-remove)
    (define-key map "u" 'slackware-upgrade)
    (define-key map "g" 'slackware-info)
    (define-key map "n" 'slackware-next-pkg)
    (define-key map "p" 'slackware-previous-pkg)
    (define-key map "q" 'slackware-quit)
    map)
  "Keymap for `slackware-mode'.")

(define-key slackware-mode-map "g" 'slackware-info)

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
        (let ((pkg (match-string-no-properties 1)))
          (message (concat "Trying install " slackware-mirror-root
                           (if (string-match "extra/" pkg)
                               pkg
                             (concat "slackware/" pkg)))))
      (message "Package not found"))))

(defun slackware-remove ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward slackware-pkg-regex nil t)
        (let ((pkg (match-string-no-properties 1)))
          (message (concat "Trying remove " slackware-mirror-root
                           (if (string-match "extra/" pkg)
                               pkg
                             (concat "slackware/" pkg)))))
      (message "Package not found"))))

(defun slackware-upgrade ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (if (re-search-forward slackware-pkg-regex nil t)
        (let ((pkg (match-string-no-properties 1)))
          (message (concat "Trying upgrade " slackware-mirror-root
                           (if (string-match "extra/" pkg)
                               pkg
                             (concat "slackware/" pkg)))))
      (message "Package not found"))))

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
