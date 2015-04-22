;;; ace-jump-helm-line.el --- Ace-jump to a candidate in helm window

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions
;; Version: 0.1
;; Package-Requires: ((ace-window "0.7.1") (helm "1.6.3"))

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

;;   You can use the following code to bind =ace-jump-helm-line=  (or
;;   =ace-jump-helm-line-execute-action=. See later) to a key(say,
;;   "C-'"):
;;   : (eval-after-load "helm"
;;   : '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))


;; * Usage
;;   When in a helm session, for example, after you call =helm-M-x=, you can use
;;   your key binding(for example, "C-'") to invoke =ace-jump-helm-line=. See demos.

;;   =ace-jump-helm-line= would just jump to that candidate in helm window. If you
;;   want to jump to and automatically select the candidate, which will then quit
;;   the helm session, you can bind the key to =ace-jump-helm-line-execute-action=.

;;   There are two kinds of styles: avy-jump style and ace-jump-mode style. By
;;   default, this package uses =avy-jump= style(anyway, it uses
;;   =avy-jump.el=!). You can certainly change to =ace-jump-mode-style= by:
;;   : (setq ace-jump-helm-line-use-avy-style nil)

;; * Acknowledgment
;;   - Thank [[https://github.com/abo-abo/ace-window/][Oleh Krehel]] for the awesome [[https://github.com/abo-abo/ace-window/][ace-window]] package.
;;   - Thank @hick for the original idea.

;;; Code:

(require 'cl-lib)
(require 'avy-jump)
(require 'helm)

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

;;;###autoload
(defun ace-jump-helm-line ()
  "Jump to a line start in helm window."
  (interactive)
  (if helm-alive-p
      (let ((orig-window (selected-window))
            (avi-keys (if ace-jump-helm-line-use-avy-style avi-keys
                        (cl-loop for i from ?a to ?z collect i))))
        (unwind-protect
            (with-selected-window (helm-window)
              (avi--goto (ace-jump-helm-line--collect-lines))
              (let ((orig-point (point)))
                (helm-previous-line)
                (unless (= (point) orig-point)
                  (helm-next-line)))))
        (select-window orig-window))
    (error "No helm session is running.")))

;;; Inspired by http://rubikitch.com/f/150416044841.ace-jump-helm-line.1.el
;;;###autoload
(defun ace-jump-helm-line-execute-action ()
  (interactive)
  (ace-jump-helm-line)
  (helm-maybe-exit-minibuffer))

(provide 'ace-jump-helm-line)
;;; ace-jump-helm-line.el ends here
