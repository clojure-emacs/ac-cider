;;; ac-cider-compliment.el --- Clojure auto-complete sources using CIDER and compliment

;; Copyright (C) 2012-2014 Alex Yakushev <alex@bytopia.org>

;; Author: Alex Yakushev <alex@bytopia.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Sam Aaron <samaaron@gmail.com>
;;
;; URL: https://github.com/alexander-yakushev/ac-cider-compliment
;; Keywords: languages, clojure, nrepl, cider, compliment
;; Version: 0.2.0
;; Package-Requires: ((cider "0.6.0") (auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a number of auto-complete sources for Clojure projects
;; using CIDER and compliment. This is a fork of ac-nrepl project by
;; Steve Purcell, with the difference that ac-cider-compliment uses
;; Compliment as a completion provider.

;;; Installation:

;; Available as a package in melpa.milkbox.net.
;; M-x package-install ac-cider-compliment

;;; Usage:

;;     (require 'ac-cider-compliment)
;;     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;     (add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'cider-mode))

;; If you want to trigger auto-complete using TAB in CIDER buffers, you may
;; want to use auto-complete in your `completion-at-point-functions':

;;     (defun set-auto-complete-as-completion-at-point-function ()
;;       (setq completion-at-point-functions '(auto-complete)))
;;     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;;
;;     (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;; Code:

(require 'cider)
(require 'auto-complete)

(defun ac-cider-compliment-available-p ()
  "Return t if CIDER supports completion, otherwise nil."
  (functionp 'cider-complete))

(defun ac-cider-compliment-candidates-everything ()
  "Return all candidates for a symbol at point."
  (setq ac-cider-compliment-documentation-cache nil)
  (cider-complete ac-prefix))

(defvar ac-cider-compliment-documentation-cache '())

(defun ac-cider-compliment-documentation (symbol)
  "Return documentation for the given SYMBOL, if available.
Caches fetched documentation for the current completion call."
  (when symbol
    (let ((cached-doc (assoc (substring-no-properties symbol)
                             ac-cider-compliment-documentation-cache)))
      (if cached-doc
          (cdr cached-doc)
        (let* ((doc
                (substring-no-properties
                 (plist-get (nrepl-send-request-sync
                             (list "op" "complete-doc"
                                   "session" (nrepl-current-session)
                                   "ns" nrepl-buffer-ns
                                   "symbol" symbol))
                            :value)))
               (doc (if (string= "\"\"" doc)
                        "No documentation available."
                      doc)))
          (add-to-list 'ac-cider-compliment-documentation-cache
                       (cons (substring-no-properties symbol) doc))
          doc)))))

(defun ac-cider-compliment-match-everything (prefix candidates)
  candidates)

;;;###autoload
(defface ac-cider-compliment-candidate-face
  '((t (:inherit ac-candidate-face)))
  "Face for nrepl candidates."
  :group 'auto-complete)

;;;###autoload
(defface ac-cider-compliment-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the nrepl selected candidate."
  :group 'auto-complete)

;;;###autoload
(defconst ac-cider-compliment-source-defaults
  '((available . ac-cider-compliment-available-p)
    (candidate-face . ac-cider-compliment-candidate-face)
    (selection-face . ac-cider-compliment-selection-face)
    (prefix . cider-completion-symbol-start-pos)
    (match . ac-cider-compliment-match-everything)
    (document . ac-cider-compliment-documentation)
    (cache))
  "Defaults common to the various completion sources.")

;;;###autoload
(defvar ac-source-compliment-everything
  (append
   '((candidates . ac-cider-compliment-candidates-everything)
     (symbol . "v"))
   ac-cider-compliment-source-defaults)
  "Auto-complete source for CIDER buffers.")

;;;###autoload
(defun ac-cider-compliment-setup ()
  "Add the Compliment completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (setq-default ac-use-fuzzy nil)
  (add-to-list 'ac-sources 'ac-source-compliment-everything))

;;;###autoload
(defun ac-cider-compliment-repl-setup ()
  "Left for backward-compatibility purposes."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-compliment-everything))

(provide 'ac-cider-compliment)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-cider-compliment.el ends here
