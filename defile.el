;;; defile.el --- Embed IDs and tags in file names   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nick OBrien

;; Author: Nick OBrien <nick4f42@proton.me>
;; Created: 2024
;; Homepage: https://github.com/nick4f42/defile.el
;; Keywords: files
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defile is a simple package for embedding IDs and tags in file
;; names.  File names are formatted as follows, with [] indicating
;; optional fields:
;;
;;   [ID--]TITLE[__TAG1_TAG2][.EXT]
;;
;; By default, Defile uses a superset of the file name format in
;; Denote <https://protesilaos.com/emacs/denote>.  In fact, if your
;; `denote-directory' is included in `defile-lookup-path', Denote
;; notes are considered as Defile files.
;;
;; To start, you should configure `defile-lookup-path'.  The main
;; entry points are `defile-rename-file', `defile-dired-do-rename',
;; and `defile-dired-mode'.  You can also use the defile Org link type
;; with `org-insert-link'.  When storing a link with `org-store-link',
;; files with a Defile ID are stored as defile links.

;;; Code:

(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

;;;; User customization

(defgroup defile nil
  "Embed IDs and tags in file names."
  :group 'files
  :prefix "defile-")

(defcustom defile-lookup-path '()
  "Directories to search for files with Defile formatted names.
The value of `defile-candidates' may consider this variable when
searching for files."
  :type '(repeat directory))

(defcustom defile-candidates #'defile-candidates-recursive
  "The files to consider when searching for Defile files.
The value must be a function that takes two arguments, HANDLE and
REGEXP.  It should (funcall HANDLE FILE) for each candidate file
name.  If REGEXP is non-nil, only files with a
`file-name-nondirectory' part that matches REGEXP should be
included."
  :type 'function)

(defcustom defile-recurse-predicate #'defile-recurse-predicate-default
  "Predicate for descending into directories when searching for files.
Only applies when `defile-candidates' is
`defile-candidates-recursive'.  This is the PREDICATE argument of
`directory-files-recursively'."
  :type '(choice (const nil) function))

(defcustom defile-id-end "--"
  "Specifies the end of the ID in a file name."
  :type 'string)

(defcustom defile-tags-start "__"
  "Specifies the start of tags in a file name."
  :type 'string)

(defcustom defile-tags-separator "_"
  "Separates tags in a file name."
  :type 'string)

(defcustom defile-extension-regexp (rx (+ "." (+ alnum)))
  "Matches file extensions."
  :type 'regexp)

(defcustom defile-new-id-function #'defile-new-id-decimal
  "A function of no arguments that returns an identifier string.
The returned string must not contain `defile-id-end'."
  :type '(choice
	  (const :tag "Ordered decimal (e.g. 0, 1, ..., 317)"
		 defile-new-id-decimal)
	  (const :tag "Random alpha-numeric (e.g. yN3sUha)"
		 (lambda ()
		   (defile-new-id-unique
		    (lambda () (defile-new-id-alnum 7))
		    8)))
	  function))

(defcustom defile-id-regexp (rx (* anychar))
  "Matches the ID part of a file name.
This is used when searching for files with Defile IDs.  By
default, an ID can be anything that does not include
`defile-id-end'.  Therefore, any file name that contains
`defile-id-end' has a Defile ID.  If you don't want that, set
this variable to a more restrictive regexp."
  :type 'regexp)

(defcustom defile-lookup-duplicate-filters
  '(defile-filter-with-user-input)
  "Functions to filter files with the same IDs.
Each function takes one argument FILES, a list of file names with
duplicate IDs.  It should either return nil to continue
processing, or a smaller list of files.  Each filter is run in
turn until there is only one file remaining."
  :type '(repeat function))

(defcustom defile-find-tags t
  "Search `defile-lookup-path' for tags when prompting for tags.
If nil, tags are still taken from `defile-tag-file'.  See also
`defile-populate-tag-file'."
  :type 'boolean)

(defcustom defile-tag-file nil
  "File to store tag names and descriptions in.
Each line in the file starts with a tag.  It can optionally be
followed with `defile-tags-start', then a description of the tag.
The tags in this file are used for completion, along with tags
already in Defile file names if `defile-find-tags' is enabled.
The description is used as an annotation when completing tags.

See also `defile-populate-tag-file'."
  :type '(choice (const nil) file))

(defgroup defile-faces nil
  "Faces for Defile."
  :group 'defile
  :prefix "defile-")

(defface defile-id '((t :inherit shadow))
  "Face for the ID part of a file name."
  :group 'defile-faces)

(defface defile-separator '((t :inherit shadow))
  "Face for the ID and tag separators in a file name."
  :group 'defile-faces)

(defface defile-tags '((t :inherit font-lock-doc-face))
  "Face for tags in a file name."
  :group 'defile-faces)

(defface defile-extension '((t :inherit shadow))
  "Face for the file extension in a file name."
  :group 'defile-faces)

;;;; Core code

(defun defile-id (file)
  "The Defile ID of a file name, or nil if there is no ID."
  (let ((name (file-name-nondirectory file)))
    (if (string-match (rx bos (group (regexp defile-id-regexp)) (literal defile-id-end))
		      name)
	(match-string 1 name))))

(defun defile-split (filename)
  "Retrieve the id, title, tags, and extension from a file name.
FILENAME must be returned by `file-name-nondirectory'."
  (let ((id-end 0)
	(title-start 0)
	(title-end (length filename))
	(ext-start (length filename))
	(tags-start (length filename))
	id title tags ext)
    (when (string-match (regexp-quote defile-id-end) filename)
      (setq id-end (match-beginning 0)
	    title-start (match-end 0)))

    (let ((i title-start))
      (while (string-match (regexp-quote defile-tags-start) filename i)
	(setq i (match-end 0)
	      title-end (match-beginning 0)
	      tags-start (match-end 0))))

    (when (string-match (rx (regexp defile-extension-regexp) eos)
			filename title-start)
      (setq ext-start (match-beginning 0)
	    title-end (min title-end ext-start)))

    (setq id (substring filename 0 id-end)
	  title (substring filename title-start title-end)
	  tags (if (> ext-start tags-start)
		   (split-string (substring filename tags-start ext-start)
				 (regexp-quote defile-tags-separator)
				 'omit-nulls))
	  ext (substring filename ext-start))

    (list id title tags ext)))

(defun defile-join (id title tags ext)
  "Combine the parts into a file name."
  (replace-regexp-in-string
   (rx (any "/" "\n" cntrl))
   ""
   (apply
    #'concat
    `(,@(if (not (string-empty-p id))
	    (list id defile-id-end))
      ,title
      ,@(if (or tags
		(string-match-p (regexp-quote defile-tags-start) title))
	    (list defile-tags-start))
      ,@(let ((s (string-join tags defile-tags-separator)))
	  (if (string-match-p (rx (regexp defile-extension-regexp) eos) s)
	      (list s defile-tags-separator)
	    (list s)))
      ,ext))))

(defun defile-lookup (id &optional filters)
  "Return a Defile file given its ID.
Files are searched for in `defile-lookup-path'."
  (let ((files nil))
    (funcall defile-candidates
	     (lambda (file)
	       (push file files))
	     (rx bos (literal id) (literal defile-id-end)))
    (cl-block nil
      (dolist (func
	       (cons nil
		     (cond ((not filters) defile-lookup-duplicate-filters)
			   ((consp filters) filters)
			   (t nil))))
	(when func
	  (setq files (or (funcall func files) files)))
	(cond ((null files)
	       (cl-return nil))
	      ((length= files 1)
	       (cl-return (car files)))))
      (error "Duplicate IDs: %S" files))))

(defun defile-tags ()
  "Return a list of known tags in `defile-lookup-path'."
  (let ((all-tags (make-hash-table :test #'equal)))
    (pcase-dolist (`(,tag . ,desc) (defile--read-tag-file))
      (puthash (propertize tag 'defile-desc desc) t all-tags))
    (when defile-find-tags
      (funcall defile-candidates
	       (lambda (file)
		 (seq-let (_id _title tags _ext) (defile-split (file-name-nondirectory file))
		   (dolist (tag tags)
		     (puthash tag t all-tags))))
	       (rx bos (regexp defile-id-regexp) (literal defile-id-end)
		   (* anychar) (literal defile-tags-start))))
    (hash-table-keys all-tags)))

(defun defile-read-tags (prompt &optional initial-tags)
  "Read a list of Defile tags with completion.
PROMPT is the `completing-read-multiple' prompt.  INITIAL-TAGS
are put in the minibuffer as initial input."
  (dlet ((crm-separator (regexp-quote defile-tags-separator))
	 (completion-extra-properties
	  (list :annotation-function #'defile--tag-annotation-function)))
    (completing-read-multiple
     prompt (defile-tags) nil nil
     (string-join initial-tags defile-tags-separator))))

(defun defile-new-file-name (file id title tags ext)
  "Change parts of FILE, possibly with user input.

ID, TITLE, TAGS, and EXT may be `prompt' to prompt the user,
`keep' to keep the old value, or the new value."
  (pcase-let*
      ((dir (file-name-directory file))
       (name (file-name-nondirectory file))
       ((seq prev-id prev-title prev-tags prev-ext) (defile-split name))
       (msg (format "Renaming %s:" name))
       (id (pcase id
	     ('keep prev-id)
	     ('prompt
	      (if (y-or-n-p (format "%s Generate ID? " msg))
		  (defile-new-id)
		""))
	     ((pred stringp) id)
	     (_ "")))
       (title (pcase title
		('keep prev-title)
		('prompt
		 (read-string (format-prompt "%s Title" prev-title msg)
			      nil nil prev-title))
		((pred stringp) title)
		(_ "")))
       (tags (defile-normalize-tags
	      (pcase tags
		('keep prev-tags)
		('prompt (defile-read-tags (format "%s Tags: "  msg) prev-tags))
		((pred listp) tags)
		(_ '()))))
       (ext (pcase ext
	      ('keep prev-ext)
	      ('prompt
	       (let ((new-ext (read-string (format-prompt "%s Extension" prev-ext msg)
					   nil nil prev-ext)))
		 (if (string-prefix-p "." new-ext)
		     new-ext
		   (concat "." new-ext))))
	      ((pred stringp) ext)
	      (_ ""))))
    (file-name-concat dir (defile-join id title tags ext))))

(defun defile-normalize-tags (tags)
  "Return the canonical form of a list of tags."
  (sort (seq-uniq tags) #'string-collate-lessp))

;;;###autoload
(defun defile-rename-file (file &optional new-id)
  "Rename FILE with a new title and tags.
When NEW-ID is non-nil (prefix argument interactively), generate
a new ID for the file.  This replaces the previous ID if one
exists."
  (interactive "fRename file: \nP")
  (let* ((id (if new-id 'prompt 'keep))
	 (new-file (defile-new-file-name file id 'prompt 'prompt 'keep)))
    (when (y-or-n-p (format "Rename %s to %s? "
			    (file-name-nondirectory file) (file-name-nondirectory new-file)))
      (rename-file file new-file))))

(defun defile--id-files ()
  "Returns a list of files with a Defile ID."
  (let ((files nil))
    (funcall defile-candidates
	     (lambda (file)
	       (push file files))
	     (rx bos (regexp defile-id-regexp) (literal defile-id-end)))
    files))

(defun defile-file-prompt ()
  "Prompt for an existing file with a Defile ID."
  (let ((file (completing-read "File: " (defile--id-files) nil t)))
    (when (string-empty-p file)
      (error "Must specify a file"))
    file))

;;;;; Candidates

(defun defile-candidates-recursive (handle regexp)
  "Return files and directories recursively from `defile-lookup-path'.
To filter out certain sub-directories, see
`defile-recurse-predicate'."
  ;; TODO: Don't store a full list of recursively found files.
  (let ((case-fold-search nil))
    (dolist (dir defile-lookup-path)
      (dolist (file (directory-files-recursively
		     dir regexp t defile-recurse-predicate))
	(funcall handle file)))))

(defun defile-recurse-predicate-default (dir)
  "Default sub-directory predicate when searching for Defile files."
  (if (boundp 'vc-directory-exclusion-list)
      (not (member (file-name-nondirectory dir) vc-directory-exclusion-list))
    t))

;;;;; IDs

(defun defile-new-id ()
  "Generate and return an ID.
An error is thrown in the generated ID is not correctly
formatted."
  (let ((id (funcall defile-new-id-function)))
    (when (string-match-p (regexp-quote defile-id-end) id)
      (error "ID %S must not contain %S" id defile-id-end))
    id))

(defun defile-new-id-unique (new-id &optional attempts)
  "Generate an IDs until a unique one is found.
NEW-ID is a function of no arguments that returns an ID.  Call it
until it returns an ID that doesn't already exist, up to a
maximum of ATTEMPTS times.  If a unique ID can't be found, throw
an error."
  (cl-block nil
    (dotimes (_ (or attempts 1))
      (let ((id (funcall new-id)))
	(unless (defile-lookup id t)
	  (cl-return id))))
    (error "Could not generate a unique ID")))

(defun defile-new-id-decimal ()
  "Generate a base 10 ID one higher than the existing maximum."
  (let ((regexp (rx bos (group (+ digit)) (literal defile-id-end)))
	(id -1))
    (funcall defile-candidates
	     (lambda (file)
	       (let ((name (file-name-nondirectory file)))
		 (when (string-match regexp name)
		   (setq id (max id (string-to-number (match-string 1 name)))))))
	     regexp)
    (format "%d" (1+ id))))

(defun defile-new-id-alnum (n &optional source)
  "Generate an alphanumeric ID of length N.
SOURCE is for internal use, do not specify it."
  (let ((id ""))
    (cl-loop
     (setq id (concat id (replace-regexp-in-string
			  (rx (not alnum)) "" (defile--base64url-hash source))))
     (when (>= (length id) n)
       (cl-return (substring id 0 n))))))

(defun defile--base64url-hash (&optional source)
  "Return the base64url encoded string of a hash."
  (base64url-encode-string
   (defile--hex-string-to-bytes
    ;; Taken from `org-id-uuid'
    (md5 (or source
	     (format "%s%s%s%s%s%s%s"
		     (random) (current-time) (user-uid) (emacs-pid)
		     (user-full-name) user-mail-address (recent-keys)))))
   'no-pad))

(defun defile--hex-string-to-bytes (hex)
  "Convert a string of hex characters into binary representation."
  (let ((i 0)
	(bytes (make-string (/ (length hex) 2) 0)))
    (while (< i (length bytes))
      (let ((j (* 2 i)))
	(aset bytes i (string-to-number (seq-subseq hex j (+ j 2)) 16)))
      (setq i (1+ i)))
    bytes))

;;;;; Filters

(defun defile-filter-with-user-input (files)
  "Prompt the user for a file in FILES."
  (list
   (completing-read
    "Duplicate IDs exist.  Choose one: " files nil t)))

;;;;; Tags

(defun defile-populate-tag-file ()
  "Add tags found in `defile-lookup-path' to `defile-tag-file'."
  (interactive)
  (or defile-tag-file
      (user-error "`defile-tag-file' is nil"))
  (with-current-buffer (find-file-noselect defile-tag-file)
    (let ((tags (defile-normalize-tags
		 (seq-filter
		  (lambda (tag)
		    (save-excursion
		      (goto-char (point-min))
		      (not (let ((case-fold-search nil))
			     (search-forward-regexp
			      (rx bol (literal tag) (or (literal defile-tags-start) eol))
			      nil t)))))
		  (defile--find-tags)))))
      (if (not tags)
	  (message "No new tags found")
	(goto-char (point-max))
	(unless (or (bobp) (eq (char-before) ?\n))
	  (insert "\n"))
	(dolist (tag tags)
	  (insert tag defile-tags-start "\n"))
	(message "Added %d tags" (length tags))))
    (pop-to-buffer (current-buffer))))

(defun defile--find-tags ()
  (let ((all-tags (make-hash-table :test #'equal)))
    (funcall defile-candidates
	     (lambda (file)
	       (seq-let (_id _title tags _ext) (defile-split (file-name-nondirectory file))
		 (dolist (tag tags)
		   (puthash tag t all-tags))))
	     (rx bos (regexp defile-id-regexp) (literal defile-id-end)
		 (* anychar) (literal defile-tags-start)))
    (hash-table-keys all-tags)))

(defun defile--tag-annotation-function (tag)
  (when-let ((desc (get-text-property 0 'defile-desc tag)))
    (concat " " desc)))

(defvar defile--cached-tag-file nil)
(defvar defile--cached-tags nil)

(defun defile--read-tag-file ()
  (when defile-tag-file
    (if (and (equal defile--cached-tag-file defile-tag-file)
	     (not (file-has-changed-p defile-tag-file 'defile--read-tag-file)))
	defile--cached-tags
      (let ((tags (with-temp-buffer
		    (insert-file-contents defile-tag-file)
		    (defile--read-tag-buffer))))
	(setq defile--cached-tag-file defile-tag-file
	      defile--cached-tags tags)
	tags))))

(defun defile--read-tag-buffer ()
  (let (tags)
    (goto-char (point-min))
    (while (search-forward-regexp
	    (rx bol
		(group (+? nonl))
		(? (literal defile-tags-start)
		   (group (* nonl)))
		eol)
	    nil t)
      (let ((tag (match-string 1))
	    (desc (match-string 2)))
	(when tag
	  (push (cons tag desc) tags))))
    (nreverse tags)))

;;;; Dired integration

(declare-function wdired-change-to-wdired-mode "wdired")
(declare-function wdired-finish-edit "wdired")

;;;###autoload
(define-minor-mode defile-dired-mode
  "Fontify all Defile-style file names.
Add this to `dired-mode-hook'."
  :global nil
  :group 'defile
  (if defile-dired-mode
      (progn
        (defile--dired-add-font-lock)
        (advice-add #'wdired-change-to-wdired-mode :after #'defile--dired-add-font-lock)
        (advice-add #'wdired-finish-edit :after #'defile--dired-add-font-lock))
    (defile--dired-remove-font-lock)
    (advice-remove #'wdired-change-to-wdired-mode #'defile--dired-add-font-lock)
    (advice-remove #'wdired-finish-edit #'defile--dired-add-font-lock))
  (font-lock-flush (point-min) (point-max)))

(defun defile--dired-add-font-lock (&rest _)
  "Add font lock keywords."
  (when (derived-mode-p 'dired-mode)
    (setq-local defile--font-lock-keywords (defile--font-lock-keywords))
    (font-lock-add-keywords nil defile--font-lock-keywords t)))

(defun defile--dired-remove-font-lock (&rest _)
  "Remove font lock keywords."
  (when (and (derived-mode-p 'dired-mode)
	     (local-variable-p 'defile--font-lock-keywords))
    (font-lock-remove-keywords nil defile--font-lock-keywords)))

(defun defile--font-lock-keywords ()
  "Keywords for fontification of Defile file names."
  (rx-let ((name-char (not (any "/" "\n")))
	   (name-end (or " -> " eol))
	   (start (seq bol (= 12 nonl) (+? nonl))))
    `((,(rx start
	    (group-n 1 (+? (not (any blank cntrl "/" "\n"))))
	    (group-n 2 (literal defile-id-end)))
       (1 'defile-id prepend t)
       (2 'defile-separator prepend t))
      (,(rx start
	    (literal defile-id-end)
	    (*? name-char)
	    (group-n 1 (literal defile-tags-start)))
       (1 'defile-separator prepend t)
       (,(rx (group-n 1 (*? name-char))
	     (or (group-n 2 (literal defile-tags-separator))
		 (regexp defile-extension-regexp)
		 name-end))
	(save-excursion
	  (save-match-data
	    (when (search-forward-regexp ,(rx name-end) nil t)
	      (match-beginning 0))))
	nil
	(1 'defile-tags prepend t)
	(2 'defile-separator prepend t)))
      (,(rx start
	    (literal defile-id-end)
	    (*? name-char)
	    (group-n 1 (regexp defile-extension-regexp)) name-end)
       (1 'defile-extension prepend t)))))

(declare-function dired-get-marked-files "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-create-files "dired-aux")
(declare-function dired-rename-file "dired-aux")

;;;###autoload
(defun defile-dired-do-rename (&optional arg)
  "Rename the current or marked files with the Defile format.
ARG has the same meaning as `dired-do-rename'."
  (interactive "P" dired-mode)
  (let ((files (dired-get-marked-files nil arg)))
    (when (seq-find (lambda (file)
                      (member (file-name-nondirectory file) '("." "..")))
                    files)
      (user-error "Can't rename \".\" or \"..\" files"))
    (dired-create-files
     #'dired-rename-file "Move" files
     (lambda (file)
       (defile-new-file-name file 'keep 'prompt 'prompt 'keep))
     ?R)))

;;;; Org integration

(defvar org-store-link-plist)
(declare-function org-link-store-props "ol")
(declare-function org-link-open-as-file "ol")

;;;###autoload
(defun defile-link-ol-follow (link)
  "Find file of type `defile:' matching LINK.
LINK is the ID of the file.  Uses `defile-lookup-path' to find
the file."
  (let ((parts (split-string link (rx "/") t)))
    (if-let ((file (defile-lookup (or (car parts) ""))))
	(org-link-open-as-file
	 (apply #'file-name-concat file (cdr parts)) nil)
      (error "Link ID does not exist: %S" link))))

;;;###autoload
(defun defile-link-ol-store ()
  "Handler for `org-store-link' adding support for `defile:' links."
  (when-let* (((derived-mode-p 'dired-mode))
	      (file (dired-get-filename))
	      (name (file-name-nondirectory file))
	      ((defile-id file)))
    (seq-let (id title) (defile-split name)
      (org-link-store-props
       :type "defile"
       :description title
       :link (concat "defile:" id))
      org-store-link-plist)))

;;;###autoload
(defun defile-link-ol-complete ()
  "Complete a Defile link."
  (let ((file (defile-file-prompt)))
    (seq-let (id _title _tags _ext) (defile-split (file-name-nondirectory file))
      (concat "defile:" id))))

;;;###autoload
(defun defile-link-ol-insert-description (link description)
  "Complete a Defile link."
  (or description
      (when (string-match (rx bos "defile:") link)
	(if-let* ((id (substring link (match-end 0)))
		  (file (defile-lookup id)))
	    (seq-let (_id title _tags _ext) (defile-split (file-name-nondirectory file))
	      title)))))

;; The `eval-after-load' part with the quoted lambda is adapted from
;; Denote: <https://github.com/protesilaos/denote/>
;; which was in turn adapted from
;; Elfeed: <https://github.com/skeeto/elfeed/>.

;;;###autoload
(eval-after-load 'org
  `(funcall
    ;; The extra quote below is necessary because uncompiled closures
    ;; do not evaluate to themselves. The quote is harmless for
    ;; byte-compiled function objects.
    ',(lambda ()
        (with-no-warnings
          (org-link-set-parameters
           "defile"
           :follow #'defile-link-ol-follow
           :store #'defile-link-ol-store
	   :complete #'defile-link-ol-complete
	   :insert-description #'defile-link-ol-insert-description)))))

(provide 'defile)
;;; defile.el ends here
