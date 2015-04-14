;;; ace-jump-helm-line.el --- Ace-jump to a candidate in helm window

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions
;; Version: 0.1
;; Package-Requires: ((ace-window "0.7.1"))

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

;; Demos: see https://github.com/cute-jumper/ace-jump-helm-line

;; #+TITLE: ace-jump-helm-line
;; *Ace-jump to a candidate in helm window.*

;; This package makes use of the =avy-jump.el= provided in [[https://github.com/abo-abo/ace-window/][ace-window]].

;; * Setup
;;   : (add-to-list 'load-path "/path/to/ace-jump-helm-line.el")
;;   : (require 'ace-jump-helm-line)

;; * Usage
;;   When in a helm session, for example, after you call =helm-M-x=, you can use
;;   "C-'" to invoke =ace-jump-helm-line=. 

;;   There are two kinds of styles: avy-jump style and ace-jump-mode style. By
;;   default, this package uses =avy-jump= style(anyway, it uses
;;   =avy-jump.el=!). You can certainly change to =ace-jump-mode-style= by:
;;   : (setq ace-jump-helm-line-use-avy-style nil)

;; * Acknowledgment
;;   - Thank [[https://github.com/abo-abo/ace-window/][Oleh Krehel]] for the awesome [[https://github.com/abo-abo/ace-window/][ace-window]] package.
;;   - Thank @hick for the original idea.

;;; Code:

(require 'avy-jump)

(defvar ace-jump-helm-line-use-avy-style t
  "Use `avy-jump' style when t. Otherwise use `ace-jump-mode' style.")

(defun ace-jump-helm-line--collect-lines ()
  "Select lines in helm window."
  (let ((avi-background (if ace-jump-helm-line-use-avy-style
                            nil t))
        candidates)
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end (selected-window) t))
        (goto-char (point-min))
        (while (or (helm-pos-header-line-p)
                   (helm-pos-candidate-separator-p))
          (forward-line 1))
        (while (< (point) (point-max))
          (push (cons (point) (selected-window))
                candidates)
          (forward-line 1)
          (while (or (helm-pos-header-line-p)
                     (helm-pos-candidate-separator-p))
            (forward-line 1)))))
    (avi--process (nreverse candidates)
                  (if ace-jump-helm-line-use-avy-style
                      #'avi--overlay-pre
                  #'avi--overlay-at))))

(defun ace-jump-helm-line ()
  "Jump to a line start in helm window."
  (interactive)
  (if helm-alive-p
      (let ((orig-window (selected-window))
            (avi-keys (if ace-jump-helm-line-use-avy-style avi-keys
                        (loop for i from ?a to ?z collect i))))
        (unwind-protect
            (with-selected-window (helm-window)
              (avi--goto (ace-jump-helm-line--collect-lines))
              (let ((orig-point (point)))
                (helm-previous-line)
                (unless (= (point) orig-point)
                  (helm-next-line)))))
        (select-window orig-window))
    (error "No helm session is running.")))

(eval-after-load "helm"
  '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))

(provide 'ace-jump-helm-line)
;;; ace-jump-helm-line.el ends here
