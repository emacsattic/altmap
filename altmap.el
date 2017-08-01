;;; altmap.el -- define alternative key bindings

;; Copyright (C) 2009  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Define alternative key bindings.

;;; Code:

(require 'keymap-utils)

(defvar altmap-alt-prefix "altmap-")
(defvar altmap-bkp-prefix "backup-")

(defvar altmap-define-mapvar-when-switching nil)
(defvar altmap-define-altvar-when-switching nil)
(defvar altmap-define-bkpvar-when-switching nil)

(defvar altmap-define-mapvar-when-defining nil)

(defvar altmap-allow-redefining-altvar t)

(defun altmap-symbol (mapvar &optional altp)
  "Return a symbol whose symbol name ends with that of MAPVAR.
If optional ALTP is non-nil `altmap-alt-prefix' is prepended otherwise
`altmap-bkp-prefix'."
  (intern (concat (if altp altmap-alt-prefix altmap-bkp-prefix)
		  (symbol-name mapvar))))

(defun altmap-define (mapvar value doc)
  "Define an alternative keymap to be used in place of MAPVAR.

The variable whose symbol name begins with the value of `altmap-alt-prefix'
and ends in the symbol name of MAPVAR it set to VALUE.  If DOC is non-nil
it is used as the variable documentation otherwise a default doc-string is
generated which refers to MAPVAR."
  (let ((altvar (altmap-symbol mapvar t))
	(fullp nil))
    (when (boundp altvar)
      (if altmap-allow-redefining-altvar
	  (message "Redefining altvar %s" altvar)
	(error "Cannot define keymap %s: it is already defined" altvar)))
    (set altvar value)
    (put altvar 'variable-documentation
	 (or doc (format "Alternative keymap to be used in place of `%s'."
			 mapvar)))
    (let ((listvar (intern (concat "altmap-" altmap-alt-prefix "mapvars"))))
      (if (boundp listvar)
	  (add-to-list listvar mapvar)
	(set listvar (list mapvar))))))

(defmacro define-altmap (mapvar &rest args)
  "Define an alternative keymap to be used in place of MAPVAR."
  ;; TODO documentation
  (declare (doc-string 2))
  (let ((altmap (altmap-symbol mapvar t))
	fn-value fn-doc
	kargs arg
	fullp copyp)
    (while (setq arg (pop args))
      (cond ((listp arg)
	     (setq fn-value (nconc fn-value (list arg))))
	    ((stringp arg)
	     (setq fn-doc arg
		   kargs args
		   args nil))
	    (t
	     (error "Junk in args %S" args))))
    (while (setq arg (pop kargs))
      (unless (keywordp arg)
	(error "Junk in args %S" args))
      (let ((keyword arg)
	    (value (car kargs)))
	(unless kargs
	  (error "Keyword %s is missing an argument" keyword))
	(setq kargs (cdr kargs))
	(case keyword
	  (:full-keymap
	   (setq fullp value))
	  (:copy-keymap
	   (setq copyp value))
	  (:substitute-key-definition) ; TODO
	  (:require) ; TODO
	  (t
	   (error "Unrecognized keyword %s" keyword)))))
    `(altmap-define
      ',mapvar
      ;; TODO handle case where mapvar isn't define yet, warn etc.
      (let* ((vanilla-var ,mapvar)
	     (vanilla-map (bound-and-true-p ,mapvar))
	     (map ,(if copyp
		       (list 'copy-keymap 'vanilla-map)
		     (list (if fullp 'make-keymap 'make-sparse-keymap)))))
	,@fn-value
	map)
      ,fn-doc)))

(defun altmap-backup (mapvar)
  "Backup the vanilla definition of MAPVAR.

This saves the cdr of MAPVAR in the cdr of the variable whose symbol name
begins with the value of `altmap-alt-prefix' and ends in the symbol name
of MAPVAR."
  (let ((bkpvar (altmap-symbol mapvar nil)))
    (if (not (boundp mapvar))
    	(error "Cannot backup keymap %s: it isn't defined" mapvar))
    (if (boundp bkpvar)
    	(error "Cannot backup keymap %s: %s is already defined" mapvar bkpvar))
    (set bkpvar (make-sparse-keymap))
    (put bkpvar 'variable-documentation
	 (format "Backup of vanilla `%s' keymap." mapvar))
    (setcar (symbol-value bkpvar) (cdr (symbol-value mapvar)))))

(defun altmap-switch (mapvar &optional altp)
  "Switch the keymap stored in MAPVAR's symbol value.

This sets the cdr of MAPVAR to the cdr of the variable whose symbol name
begins with the value of `altmap-alt-prefix' or `altmap-bkp-prefix' and
ends in the symbol name of MAPVAR.

If ALTP is non-nil the value of `altmap-alt-prefix' is used to determine
the variable whose value is used otherwise the value of
`altmap-bkp-prefix' is used."
  (let ((altvar (altmap-symbol mapvar t))
	(bkpvar (altmap-symbol mapvar nil)))
    (when (not (boundp mapvar))
      (if (not altmap-define-mapvar-when-switching)
	  (error "Cannot switch keymap %s: it is not defined" mapvar)
	(message "Switching keymap %s: which wasn't defined yet" mapvar)
	(set mapvar (make-sparse-keymap))))
    (when (not (boundp altvar))
      (if (not altmap-define-altvar-when-switching)
	  (error "Cannot switch keymap %s: %s is not defined" mapvar altvar)
	(message "Switching keymap %s: altvar wasn't defined" mapvar)
	(altmap-define mapvar)))
    (when (not (boundp bkpvar))
      (if (not altmap-define-bkpvar-when-switching)
    	  (error "Cannot switch keymap %s: %s is not defined" mapvar bkpvar)
    	(message "Switching keymap %s: bkpvar wasn't defined" mapvar)
    	(altmap-backup mapvar)))
    (kmu-set-mapvar mapvar (default-value (if altp altvar bkpvar)))))

(defun altmap-load-directory (directory &optional recursivep)
  "Load keymaps from files in DIRECTORY.

Files ending in \".el\" in DIRECTORY and if optional RECURSIVEP is non-nil
it's subdirectories are loaded in alphabetic order using `load-file'.
Files and directories starting with \".\" or \"_\" are ignored.

If a directory contains a file \".altmap\" then instead of loading all
matching files in the directory alphabetically load the files listed in
this file in the specified order.  This file should contain a whitespace
separeted list of file names relative to the containing directory.

This function does not care about the content of the files it loads but it
is recommended that you do not use this function for anything but loading
keybindings from files in a dedicated tree."
  (let* ((contents (directory-files directory nil "^\\([^._]\\|\\.altmap\\)"))
	 (orderfile (car (member ".altmap" contents))))
    (when orderfile
      (with-temp-buffer
	(insert-file-contents
	 (concat (file-name-as-directory directory) orderfile))
	(setq contents (read (buffer-string)))))
    (dolist (file contents)
      (setq file (concat (file-name-as-directory directory) file))
      (unless (file-exists-p file)
	(setq file (concat file ".el")))
      (cond ((and (file-regular-p file)
		  (string-match "\\.el$" file))
	     (load-file file))
	    ((and (or recursivep orderfile)
		  (file-directory-p file))
	     (altmap-load-directory file recursivep))))))

(provide 'altmap)
;;; altmap.el ends here
