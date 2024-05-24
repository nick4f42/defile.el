;;; defile-tests.el --- Test suite for defile.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nick OBrien

;; Author: Nick OBrien <nick4f42@proton.me>
;; Keywords:

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

;;; Code:

(require 'defile)
(require 'ert)

;;;; Utilities

(defmacro defile--with-defaults (&rest body)
  (declare (indent 0) (debug t))
  `(let ((defile-lookup-path nil)
	 (defile-candidates nil)
	 (defile-id-end "--")
	 (defile-tags-start "__")
	 (defile-tags-separator "_")
	 (defile-extension-regexp (rx (+ "." (+ alnum))))
	 (defile-new-id-function nil)
	 (defile-id-regexp (rx (* anychar)))
	 (defile-lookup-duplicate-filters nil)
	 (defile-find-tags t)
	 (defile-tag-file nil))
     ,@body))

(defun defile--candidates (files)
  (lambda (handle regexp)
    (let ((case-fold-search nil))
      (dolist (file files)
	(when (or (not regexp)
		  (string-match-p regexp (file-name-nondirectory file)))
	  (funcall handle file))))))

;;;; Tests

(ert-deftest defile-id ()
  (defile--with-defaults
    (should (equal (defile-id "--") ""))
    (should (equal (defile-id "--title") ""))
    (should (equal (defile-id "id--title") "id"))
    (should (equal (defile-id "title") nil))
    (let ((defile-id-regexp (rx (+ digit))))
      (should (equal (defile-id "123--title") "123"))
      (should (equal (defile-id "id--title") nil)))))

(ert-deftest defile-split ()
  (defile--with-defaults
    (should (equal (defile-split "foo-_bar")
		   '("" "foo-_bar" () "")))
    (should (equal (defile-split "foo_-bar.ext.gz")
		   '("" "foo_-bar" () ".ext.gz")))
    (should (equal (defile-split "id--title")
		   '("id" "title" () "")))
    (should (equal (defile-split "--title")
		   '("" "title" () "")))
    (should (equal (defile-split "i __d-- title.com __tag.me_2_.ext")
		   '("i __d" " title.com " ("tag.me" "2") ".ext")))
    (should (equal (defile-split "title__tag-A_tag B")
		   '("" "title" ("tag-A" "tag B") "")))
    (should (equal (defile-split "title__tag-A_tag B.ext.gz")
		   '("" "title" ("tag-A" "tag B") ".ext.gz")))
    (should (equal (defile-split "title__weird.tag_.ext.gz")
		   '("" "title" ("weird.tag") ".ext.gz")))
    (should (equal (defile-split "long__title__.ext")
		   '("" "long__title" () ".ext")))
    (should (equal (defile-split "long__title__tag.ext")
		   '("" "long__title" ("tag") ".ext")))))

(ert-deftest defile-join ()
  (defile--with-defaults
    (should (equal (defile-join "" "title" '() "")
		   "title"))
    (should (equal (defile-join "" "title" '() ".ext")
		   "title.ext"))
    (should (equal (defile-join "id" "title" '() "")
		   "id--title"))
    (should (equal (defile-join "" "title" '("tag-A" "tag B") "")
		   "title__tag-A_tag B"))
    (should (equal (defile-join "" "title" '("weird.tag") ".ext")
		   "title__weird.tag_.ext"))
    (should (equal (defile-join "" "title__not tag!" '() ".ext")
		   "title__not tag!__.ext"))
    (should (equal (defile-join "no\n" "bad/" '("chars\0") ".ext")
		   "no--bad__chars.ext"))))

(ert-deftest defile-lookup ()
  (defile--with-defaults
    (let ((defile-candidates
	   (defile--candidates
	    '("a/id1--foo"
	      "a/junk"
	      "b/id2--bar"
	      "b/junk"
	      "c/junk"
	      "d/dup--one.txt"
	      "e/dup--two.txt~"))))
      (should (equal (defile-lookup "id1" t) "a/id1--foo"))
      (should (equal (defile-lookup "id2" t) "b/id2--bar"))
      (should (equal (defile-lookup "id3" t) nil))
      (should (string-match-p
	       (rx "Duplicate ID")
	       (cadr (should-error (defile-lookup "dup")))))
      (should (equal (defile-lookup
		      "dup"
		      `(,(lambda (files)
			   (seq-filter
			    (lambda (file) (not (string-match-p (rx "~" eos) file)))
			    files))))
		     "d/dup--one.txt")))))

(ert-deftest defile-tags ()
  (defile--with-defaults
    (cl-flet ((norm (tags) (sort (copy-sequence tags) #'string-lessp)))
      (let ((defile-candidates
	     (defile--candidates
	      '("a/no-id__bad1__bad2"
		"a/id1--title__tag1_tag2.txt"
		"a/id2--title__tag2__tag3.ogg"
		"b/id3--title__tag4"
		"c/id4--__foo bar"))))
	(should (equal (norm (defile-tags))
		       (norm '("tag1" "tag2" "tag3" "tag4" "foo bar"))))))))

(ert-deftest defile-new-file-name ()
  (defile--with-defaults
   (should
    (equal (defile-new-file-name "old.txt" "new-id" "title" '("t1" "t2") ".ext")
	   "new-id--title__t1_t2.ext"))
   (should
    (equal (defile-new-file-name "oldid--oldtitle__oldtag.txt" "newid" 'keep 'keep 'keep)
	   "newid--oldtitle__oldtag.txt"))
   (should
    (equal (defile-new-file-name "oldid--oldtitle__oldtag.txt" 'keep "newtitle" 'keep 'keep)
	   "oldid--newtitle__oldtag.txt"))
   (should
    (equal (defile-new-file-name "oldid--oldtitle__oldtag.txt" 'keep 'keep '("newtag") 'keep)
	   "oldid--oldtitle__newtag.txt"))
   (should
    (equal (defile-new-file-name "oldid--oldtitle__oldtag.txt" 'keep 'keep 'keep ".org")
	   "oldid--oldtitle__oldtag.org"))))

(ert-deftest defile-new-id ()
  (defile--with-defaults
    (let ((defile-new-id-function (lambda () (concat "uh oh " defile-id-end))))
      (should (string-match-p
	       (rx "must not contain")
	       (cadr (should-error (defile-new-id))))))))

(ert-deftest defile-new-id-unique ()
  (defile--with-defaults
    (let* ((id-pool (list "a" "b"))
	   (new-id (lambda () (or (pop id-pool) "c")))
	   (attempts 3)
	   (defile-candidates
	    (defile--candidates '("a--taken" "b--me-too" "id--foo"))))
      (should (equal (defile-new-id-unique new-id attempts) "c")))
    (let ((new-id (lambda () "duplicate"))
	  (defile-candidates
	   (defile--candidates '("a/id1--foo" "b/duplicate--bar"))))
      (should (string-match-p
	       (rx "Could not generate a unique ID")
	       (cadr (should-error (defile-new-id-unique new-id))))))))

(ert-deftest defile-new-id-decimal ()
  (defile--with-defaults
    (let ((defile-candidates
	   (defile--candidates
	    '("1--foo" "13--nice" "02--bar" "15-nope" "nope-16--title"))))
      (should (equal (defile-new-id-decimal) "14")))
    (let ((defile-candidates
	   (defile--candidates '("id--foo"))))
      (should (equal (defile-new-id-decimal) "0")))))

(ert-deftest defile-new-id-alnum ()
  (should (equal (defile-new-id-alnum 30 "https://xkcd.com/221/")
		 "N1bnqLlaCg7F2MUNB0aGBAN1bnqLla")))

(ert-deftest defile--hex-string-to-bytes ()
  (dotimes (i #xff)
    (should (equal (vconcat (defile--hex-string-to-bytes (format "b3%02xa2" i)))
		   (vector #xb3 i #xa2))))
  (should (equal (vconcat (defile--hex-string-to-bytes "deadbeef"))
		 [#xde #xad #xbe #xef]))
  (should (equal (vconcat (defile--hex-string-to-bytes ""))
		 [])))

(ert-deftest defile--read-tag-buffer ()
  (with-temp-buffer
    (insert "\n"
	    "tag1__desc_1__foo\n"
	    "tag2__desc2\n"
	    "\n"
	    "tag3__\n"
	    "tag4\n"
	    "tag5__desc 5")
    (should (equal (defile--read-tag-buffer)
		   '(("tag1" . "desc_1__foo")
		     ("tag2" . "desc2")
		     ("tag3" . "")
		     ("tag4" . nil)
		     ("tag5" . "desc 5"))))))

(provide 'defile-tests)
;;; defile-tests.el ends here
