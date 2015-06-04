;;; ace-jump-helm-line.el --- Ace-jump to a candidate in helm window

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; URL: https://github.com/cute-jumper/ace-jump-helm-line
;; Keywords: extensions
;; Version: 0.3.2
;; Package-Requires: ((avy "0.2.0") (helm "1.6.3"))

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

;;                           ____________________

;;                            ACE-JUMP-HELM-LINE

;;                               Junpeng Qiu
;;                           ____________________


;; Table of Contents
;; _________________

;; 1 Setup
;; 2 Usage
;; 3 Acknowledgment


;; Ace-jump to a candidate in helm window.

;; This package makes use of the `avy.el'.


;; 1 Setup
;; =======

;;   ,----
;;   | (add-to-list 'load-path "/path/to/ace-jump-helm-line.el")
;;   | (require 'ace-jump-helm-line)
;;   `----

;;   You can use the following code to bind `ace-jump-helm-line' (or
;;   `ace-jump-helm-line-execute-action'. See later) to a key(say, "C-'"):
;;   ,----
;;   | (eval-after-load "helm"
;;   | '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))
;;   `----


;; 2 Usage
;; =======

;;   When in a helm session, for example, after you call `helm-M-x', you
;;   can use your key binding(for example, "C-'") to invoke
;;   `ace-jump-helm-line'. See demos.

;;   `ace-jump-helm-line' would just jump to that candidate in helm window.
;;   If you want to jump to and automatically select the candidate, which
;;   will then quit the helm session, you can bind the key to
;;   `ace-jump-helm-line-execute-action'.

;;   There are two kinds of styles: avy-jump style and ace-jump-mode style.
;;   By default, this package uses `avy-jump' style(anyway, it uses
;;   `avy-jump.el'!). You can certainly change to `ace-jump-mode-style' by:
;;   ,----
;;   | (setq ace-jump-helm-line-use-avy-style nil)
;;   `----


;; 3 Acknowledgment
;; ================

;;   - Thank [Oleh Krehel] for the awesome [avy] package.
;;   - Thank @hick for the original idea.


;;   [Oleh Krehel] https://github.com/abo-abo/

;;   [avy] https://github.com/abo-abo/avy

;;; Code:

(require 'avy)
(require 'helm)

(defvar ace-jump-helm-line-use-avy-style t
  "Use `avy-jump' style when t. Otherwise use `ace-jump-mode' style.")

(defun ace-jump-helm-line--collect-lines ()
  "Select lines in helm window."
  (let ((avy-background (if ace-jump-helm-line-use-avy-style
                            nil t))
        avy-all-windows
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
    (avy--process (nreverse candidates)
                  (if ace-jump-helm-line-use-avy-style
                      #'avy--overlay-pre
                  #'avy--overlay-at))))

;;;###autoload
(defun ace-jump-helm-line ()
  "Jump to a line in helm window."
  (interactive)
  (if helm-alive-p
      (let ((orig-window (selected-window))
            (avy-keys (if ace-jump-helm-line-use-avy-style avy-keys
                        (number-sequence ?a ?z))))
        (unwind-protect
            (with-selected-window (helm-window)
              (avy--goto (ace-jump-helm-line--collect-lines))
              (let ((orig-point (point)))
                (helm-previous-line)
                (unless (= (point) orig-point)
                  (helm-next-line)))))
        (select-window orig-window))
    (error "No helm session is running.")))

;;; Inspired by http://rubikitch.com/f/150416044841.ace-jump-helm-line.1.el
;;;###autoload
(defun ace-jump-helm-line-execute-action ()
  "Jump to a line and execute persistent action in helm window."
  (interactive)
  (ace-jump-helm-line)
  (helm-maybe-exit-minibuffer))

(provide 'ace-jump-helm-line)
;;; ace-jump-helm-line.el ends here
