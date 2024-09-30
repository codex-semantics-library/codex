;;; tracelog-mode.el --- Interaction with tracelog files -*- lexical-binding: t; -*-
;; Copyright (C) 2024       CEA (Commissariat à l'énergie atomique et
;;                               aux énergies alternatives)
;; Author: Matthieu Lemerre <matthieu.lemerre@cea.fr>
;; Keywords: files, outlines, tools, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;  This file is part of the Codex semantics library.                     ;
;                                                                        ;
;  Copyright (C) 2013-2024                                               ;
;    CEA (Commissariat à l'énergie atomique et aux énergies              ;
;         alternatives)                                                  ;
;                                                                        ;
;  you can redistribute it and/or modify it under the terms of the GNU   ;
;  Lesser General Public License as published by the Free Software       ;
;  Foundation, version 2.1.                                              ;
;                                                                        ;
;  It is distributed in the hope that it will be useful,                 ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU Lesser General Public License for more details.                   ;
;                                                                        ;
;  See the GNU Lesser General Public License version 2.1                 ;
;  for more details (enclosed in the file LICENSE).                      ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;; Tracelog files contain a log structured as a tree.  Taking
;; advantage of the flexibility provided by outline-mode, this mode
;; provides commands to navigate this tree strucure (folding, go to
;; parent/sibbling, etc.). It also allows the handling of buttons used
;; to navigate between different parts of the log, or to view
;; additional information that cannot be put in the log.  Finally, it
;; highlights different pars of the log, e.g. to better distinghish
;; the different log levels.

;; Summary of implemented features:
;; - DONE Motion and folding of trace nodes
;; - DONE Syntax highlighting (of the guide lines and categories)
;; - DONE Syntax highlighting for the log levels (note, warning, error)
;; - DONE Buttons to access data that is not in the log, after
;;   declaration of the link type in the header.
;; - TODO Some a posteriori filtering, making invisible or dimming
;;   some items etc using transient, or opening nodes that match a
;;   regexp.
;; - MAYBE: Number of total warnings and errors in the mode line, like
;;   in compilation mode.  (maybe we should derive from compilation
;;   mode).

;;; Code:

(require 'outline)
(require 'ring)

;; The following regexps shoule be optimized to the different uses.

;; Regexp used to color guide lines: does not match the empty string.
(defvar tracelog-must-guide-regexp
  "^\\([├└]─\\|│ \\(?:│ \\)*\\(?:[├└]─\\)?\\)")

;; Faster version: most likely alternative last, no backtrack.
(defvar tracelog-outline-regexp
  "^\\(\\[\\|[├└]─\\[\\|│ \\(?:│ \\)*[├└]─\\[\\)")


;; Match a category and put the result in group 1.
(defvar tracelog-category-regexp "\\(?1:\\[.*?\\]\\)")

;; Similarly to tracelog-outline-regexp, we quickly match on three
;; alternatives.
(defvar tracelog-guide-and-category-regexp
  (concat
   "^\\(?:"
   tracelog-category-regexp
   "\\|[├└]─" tracelog-category-regexp
   "\\|│ \\(?:│ \\)*[├└]─" tracelog-category-regexp
   "\\)"))

;; Everything between Note and : is is bold underlined
(defvar tracelog-note-regexp "Note [^:]*?:")

;; Everything between Warning and : is underlined and in 'warning face
;; (orange by default)
(defvar tracelog-warning-regexp "Warning [^:]*?:")

;; Everything between Error or Alarm or Critical and : is underlined
;; and in 'error face (bright red by default)
(defvar tracelog-error-regexp "\\(Error\\|Alarm\\|Critical\\) [^:]*?:")


;; Taken from compilation-mode. We take the GNU standard of
;; representing position in files as
;;   FILE:LINE[-ENDLINE]:[COL[-ENDCOL]], enclosed in <>.
(defvar tracelog-gnu-position
  (concat "<"
	  (rx
       ;; File name group.
       (group-n 1
         ;; Avoid matching the file name as a program in the pattern
         ;; above by disallowing file names entirely composed of digits.
         ;; Do not allow file names beginning with a space.
         (| (not (in "0-9" "\n\t "))
            (: (+ (in "0-9"))
               (not (in "0-9" "\n"))))
         ;; A file name can be composed of any non-newline char, but
         ;; rule out some valid but unlikely cases, such as a trailing
         ;; space or a space followed by a -, or a colon followed by a
         ;; space.
         (*? (| (not (in "\n :"))
                (: " " (not (in ?- "/\n")))
                (: ":" (not (in " \n"))))))
       ":" (? " ")

       ;; Line number group.
       (group-n 2 (+ (in "0-9")))
       (? (| (: "-"
                (group-n 4 (+ (in "0-9")))               ; ending line
                (? "." (group-n 5 (+ (in "0-9")))))      ; ending column
             (: (in ".:")
                (group-n 3 (+ (in "0-9")))               ; starting column
                (? "-"
                   (? (group-n 4 (+ (in "0-9"))) ".")    ; ending line
                   (group-n 5 (+ (in "0-9")))))))        ; ending column
       ) ">"))

;; (defvar tracelog-button-regexp "<(\\([a-zA-Z0-9:/._- %=]\\)+)>"
;;   "Regular expression used to recognized buttons in tracelog buffers.")
(defvar tracelog-button-regexp "<(\\(.*?\\))>"
  "Regular expression used to recognized buttons in tracelog buffers.")

(defvar tracelog-button-define-regexp "<(define:\\(.*?\\))>"
  "Regular expression used to recognized define buttons.")


(defvar tracelog-history (make-ring 100)
  "Ring to keep track of navigation history.")

(defun tracelog-push-mark ()
  "Push the current point to the tracelog history ring."
  (ring-insert tracelog-history (point-marker)))


(defun tracelog-pop-mark ()
  "Pop the last navigation marker from the tracelog history ring and jump to it."
  (interactive)
  (let ((prev-marker (ring-remove tracelog-history 0)))
    (if prev-marker
        (progn
          (goto-char (marker-position prev-marker))
          (recenter))
      (message "No previous location to go back to"))))




(defun tracelog-outline-level ()
  "The number is the number of characters matched by `outline-regexp'."
  (let ((len (- (match-end 0) (match-beginning 0))))
    len))

(defun tracelog-outline-font-lock-face ()
  "Return one of `outline-font-lock-faces' for current level."
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at tracelog-outline-regexp)
    (aref outline-font-lock-faces
          (% (/(- (match-end 0) (match-beginning 0)) 2)
             (length outline-font-lock-faces)))))

;; Move point to the opening brace in [
(defun tracelog-move-to-category ()
  "Move point to the opening square braquet ('[') of a category."
  (beginning-of-line)
  (looking-at tracelog-outline-regexp)
  (goto-char (- (match-end 0) 1)))


(defun tracelog-next-visible-heading (arg)
  "Move to the next log item.  With ARG, repeat."
  (interactive "p")
  (outline-next-visible-heading arg)
  (tracelog-move-to-category))

(defun tracelog-previous-visible-heading (arg)
  "Move to the previous log item.  With ARG, repeat."
  (interactive "p")
  (outline-previous-visible-heading arg)
  (tracelog-move-to-category))

(defun tracelog-backward-same-level (arg)
"Move backward to the ARG’th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-backward-same-level arg)
  (tracelog-move-to-category))

(defun tracelog-forward-same-level (arg)
"Move forward to the ARG’th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-forward-same-level arg)
  (tracelog-move-to-category))



(defun tracelog-up-heading (arg)
  "Move to the visible heading line whose present line is a subheading.
If ARG, repeat."
  (interactive "p")
  (outline-up-heading arg)
  (tracelog-move-to-category))


(defvar tracelog-mode-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 10)
      (define-key map `[,(+ ?0 i)] 'digit-argument))
    (define-key map "-" 'negative-argument)

    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
;    (define-key map (kbd "TAB") #'outline-toggle-children)
    (define-key map (kbd "RET") #'outline-cycle)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "backtab") #'backward-button)
    (define-key map "p" 'tracelog-previous-visible-heading)
    (define-key map "n" 'tracelog-next-visible-heading)
    (define-key map "f" 'tracelog-forward-same-level)
    (define-key map "b" 'tracelog-backward-same-level)
    (define-key map "u" 'tracelog-up-heading)
    (define-key map (kbd "<") #'beginning-of-buffer)
    (define-key map (kbd ">") #'end-of-buffer)
    (define-key map (kbd "C-c C-k") #'kill-buffer)
    (define-key map "l" 'tracelog-pop-mark)    	; go back in the buffer.
    (define-key map "r" 'tracelog-goto-root)
    (define-key map "q" 'quit-window)    ; or quit-window
    map))


;; Note: I could probably not use font-lock, and fontify the buffer once instead.
(defvar tracelog-mode-font-lock-keywords
  `((,tracelog-must-guide-regexp 0 'shadow)
;    (,tracelog-guide-and-category-regexp 1 (tracelog-outline-font-lock-face))
    (,tracelog-note-regexp 0 '(bold underline))
    (,tracelog-warning-regexp 0 '(warning underline))
    (,tracelog-error-regexp 0 '(error underline))
    ))



(defun tracelog-search-for-define ()
  "As we buttonize with font-lock lazily, we could miss some define buttons.
To avoid this, this function early-searches for all define
buttons in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward tracelog-button-define-regexp nil t)
      (let* ((inhibit-read-only t)
	     (start (match-beginning 0))
             (end (match-end 0))
             (text (match-string 0))
	     (l (split-string text ":" nil "\\(<(\\|)>\\)")))
	(pcase l
	  (`("define" . (,newname . (,oldname . ,args)))
	   (apply 'tracelog-button/define start end newname oldname args))
	  (_ (error "Wrong number of arguments in a define button")))))))

(define-derived-mode tracelog-mode outline-mode "Tracelog"
  "Interaction with a tracelog file or buffer."
					;  :keymap tracelog-mode-map
  (progn
    (remove-overlays)
    (buffer-disable-undo)
    (tracelog-search-for-define)
    (setq outline-regexp tracelog-outline-regexp)
    (setq-local outline-minor-mode-use-buttons 'in-margins) ;;either
    (setq-local outline-minor-mode-cycle t)
    (setq-local font-lock-defaults '(tracelog-mode-font-lock-keywords t))
    ;; Still slow on large files. It seems that the culprit is having a large invisible text,
    ;; not having buttons.
    ;; Actually, what is slow is having to display a large portion of invisible text; i.e.,
    ;; folding a large amount of text. Maybe I should just remove invisible text
    ;; (and insert it back by copying the contents from the file, using insert-file-contents)
    ;; But then I will not be able to search for it.
    ;; Maybe: test and ask for solutions on the emacs mailing list?
;    (tracelog-buttonize-region 0 (point-max))
;    (tracelog-categories-to-button-in-region 0 (point-max))
     (jit-lock-register #'tracelog-categories-to-button-in-region)
     (jit-lock-register #'tracelog-buttonize-region)
    ))


(define-button-type 'tracelog-button 'help-echo "My message")
(define-button-type 'tracelog-category-button
  'help-echo "RET, mouse-1: expand/collapse node"
  'action 'outline-cycle
  )

(defun tracelog-button/pos-action (button)
  "Execute action for the <(pos:...)> button BUTTON, i.e. goto position."
  (let ((file (button-get button 'file))
	(line (button-get button 'line))
	(column (button-get button 'column)))
    (find-file file)
    (goto-line line)
    (if column (move-to-column column))
))

(defun tracelog-button/pos (start end file line &optional column)
  "Buttons <pos:file:line> and <pos:file:line:column> allows
visiting files.."
  (apply 'make-text-button start end :type 'tracelog-button
		    'help-echo (format "Go to %s line %d column %s" file (string-to-number line) column) 
		    'action 'tracelog-button/pos-action
		    'file file
		    'line (string-to-number line)
		    (if column
			(list 'column (string-to-number column)))))

;; URL: DONE
(defun tracelog-button/url-action (button)
  "Action for tracelog url buttons (browse url)"
  (let ((url (button-get button 'url)))
    (browse-url url)))

(defun tracelog-button/url (start end &rest url)
  "Buttons <url:...> and visit url with browse-url."
  (let ((url (string-join url ":")))
    (make-text-button start end :type 'tracelog-button
		      'help-echo (format "Browse URL %s" url)
		      'url url
		      'action 'tracelog-button/url-action)))


(defun tracelog-button/search-before-action (button)
  "Action for tracelog url buttons (browse url)"
  (let ((target (button-get button 'target)))
    (tracelog-push-mark)
    (message
     (substitute-command-keys "Position saved to the mark ring; pop it with \\[tracelog-pop-mark]."))
    (search-backward target)))

(defun tracelog-button/search-before (start end pattern &rest args)
  "Create a <(search-before:text)> button that jumps to some previous text.
Text can contain %1,%2... placeholders that are replaced by the following
 arguments.  For instance,<(search-before:%1,%2:Hello:World)> jumps to the
 previous Hello, World string."
  (let ((target pattern)
	(index 0))
     (while (string-match "%[0-9]+" target)
      (setq index (1- (string-to-number (substring (match-string 0 target) 1))))
      (setq target (replace-match (format "%s" (nth index args)) t t target)))
     target
    (make-text-button start end :type 'tracelog-button
		      'help-echo (format "Goes to previous %s" target)
		      'target target
		      'action 'tracelog-button/search-before-action)))


(defun tracelog-button-error (start end &rest _args)
  "Problematic buttons are highlighted with this function."
  (add-text-properties start end '(font-lock-face '(custom-invalid)))
  (add-text-properties start end '(help-echo "Unknown button type")))

(defun tracelog-button/define (start end newname oldname &rest args)
  "Create a define button.
The text between START and END should be of the form
<(define:NEWNAME:OLDNAME:ARGS...)>.  Clicking on these buttons
has no effect.  Instead, their presence allows a tracelog file to
define new kind of buttons, that are specialized versions of
existing buttons."
  (let ((newsymbol (intern (concat "tracelog-button/" newname)))
	(oldsymbol (intern (concat "tracelog-button/" oldname))))
    (make-local-variable newsymbol)
    (set newsymbol (lambda (start end &rest tail)
		     (apply oldsymbol start end (append args tail))))
    (make-text-button start end :type 'tracelog-button)
    ))



;; Transform all the elemens matching the <...> regexp into regions.
(defun tracelog-buttonize-region (start end)
  "For each button <name:...> of type name, transform it into button by calling
the function tracelog-button/name with the start end end
position, and the list of arguments.  See existing buttons for examples."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
	  (end-line (progn (goto-char end) (line-end-position)))
	  (inhibit-read-only t))
      (goto-char beg-line)
      (while (re-search-forward tracelog-button-regexp end-line t)
	(let* ((start (match-beginning 0))
               (end (match-end 0))
               (text (match-string 0))
	       (l (split-string text ":" nil "\\(<(\\|)>\\)"))
	       (symbol (intern (concat "tracelog-button/" (car l))))
	       )
	  ;; We cannot create buffer-local functions because we are a lisp-2, but
	  ;; we can create buffer-local variables that contain lexical closures.
	  (cond
	   ((functionp symbol)
	    (apply symbol start end (cdr l)))
	   ((and (boundp symbol) (functionp (symbol-value symbol)))
	    (apply (symbol-value symbol) start end (cdr l)))
	   (t (tracelog-button-error start end)
	      )))))))

(defun tracelog-categories-to-button-in-region (start end)
  "For each [category] transform it into a category button, so that
you can use tab/backtab to switch to it, and open/collapse it
using the mouse."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
	  (end-line (progn (goto-char end) (line-end-position)))
	  (len-outline-font-lock-faces (length outline-font-lock-faces))
	  (inhibit-read-only t))
      (goto-char beg-line)
      (while (re-search-forward tracelog-guide-and-category-regexp end-line t)
	(let* ((cat-begin (match-beginning 1))
               (cat-end (match-end 1))
	       (level (/ (- cat-begin (match-beginning 0)) 2))
	       (face (aref outline-font-lock-faces
			   (% level len-outline-font-lock-faces))))
	  ;; TODO: I could associate the status, open or closed, to the button, to remember it when folding/unfolding.
	  (make-text-button cat-begin cat-end
			    :type 'tracelog-category-button
			    'face face
			    'level level
			    ))))))


;; Experiment: also slow. I suspect that redisplay of large quantities of
;; invisible text is always slow.
(defun tracelog-text-make-invisible ()
  (interactive)
  (save-excursion
    (let ((start (point)) end)
      (tracelog-forward-same-level 1)
      (setq end (point))
      (put-text-property start end 'invisible t))))


(provide 'tracelog-mode)
;;; tracelog-mode.el ends here
