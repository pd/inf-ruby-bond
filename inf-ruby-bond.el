(defun inf-ruby-bond--escape (s)
  "Escapes a string such that it will be acceptable within a single-quoted ruby string"
  (replace-regexp-in-string "'" "\\'" (or s "") nil 'literal))

(defvar inf-ruby-bond--ruby-word-break-chars
  " \t\n\"\'`><;|&{(")

(defun inf-ruby-bond--bounds-of-ruby-word-at-point ()
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward (concat "^" inf-ruby-bond--ruby-word-break-chars))
      (list (point) end))))

(defun inf-ruby-bond--ruby-word-at-point ()
  (let ((bounds (inf-ruby-bond--bounds-of-ruby-word-at-point)))
    (buffer-substring (car bounds) (cadr bounds))))

(defun inf-ruby-bond-completions (word line)
  "Returns a list of bond's completions for WORD in the full line LINE.

Replaces inf-ruby-completions."
  (let* ((proc (inf-ruby-proc))
         (comint-filt (process-filter proc))
         (word (inf-ruby-bond--escape word))
         (line (inf-ruby-bond--escape line))
         (kept "")
         completions)
    (set-process-filter proc (lambda (proc string)
                               (setq kept (concat kept string))))
    (process-send-string proc (format "puts Bond.agent.call('%s', '%s')\n" word line))
    (while (not (string-match inf-ruby-prompt-pattern kept))
      (accept-process-output proc))
    (setq completions (butlast (split-string kept "[\r\n]") 2))
    (set-process-filter proc comint-filt)
    completions))

(defun inf-ruby-bond-complete-or-tab (&optional command)
  "Either complete the ruby code at point or call
`indent-for-tab-command' if no completion is available.  Relies
on Bond completion having been loaded and started, typically
from irbrc:
    require 'bond'
    Bond.start"
  (interactive (list (let* ((word (inf-ruby-bond--ruby-word-at-point))
                            (line (thing-at-point 'line))
                            (completions (inf-ruby-bond-completions word line)))
                       (case (length completions)
                         (0 nil)
                         (1 (car completions))
                         (t (completing-read "possible completions: "
                                             completions nil 'confirm-only word))))))
  (if (not command)
      (call-interactively 'indent-for-tab-command)
    (let ((wbounds (inf-ruby-bond--bounds-of-ruby-word-at-point)))
      (kill-region (car wbounds) (cadr wbounds)))
    (insert command)))

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-mode-map (kbd "TAB") 'inf-ruby-bond-complete-or-tab))

(provide 'inf-ruby-bond)
