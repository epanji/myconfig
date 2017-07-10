;; Copyright (C) 2017  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Version: 0.0.2
;; Created: 04 June 2017
;; Keywords: codeigniter ci service web json qzuma

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; macros

(defmacro qz-line (ntab neol &rest body)
  "Create new line with numbered tab and numbered end-of-line.
The argument to this command are as follow:

NTAB: an integer of character tab.
NEOL: an integer of character end-of-line.
BODY: sequences of argument which each argument may be a string or a
      list or vector of characters (integers)list of string or
      expression that return string body of line."
  `(let ((n1 ,ntab)
         (n2 ,neol))
     (unless (integerp n1)
       (setq n1 0))
     (unless (integerp n2)
       (setq n2 0))
     (concat (make-string n1 ?\t) ,@body (make-string n2 ?\n))))

;;; buffers

(defun qz-open-clear-buffer (name)
  "Open buffer with specific name."
  (switch-to-buffer-other-window name)
  (erase-buffer))

(defun qz-open-continue-buffer (name &optional char)
  "Open existing buffer and set point to continue."
  (unless (characterp char)
    (setq char ?\}))
  (switch-to-buffer-other-window name)
  (unless (equal 1 (goto-char (point-max)))
    (search-backward (char-to-string char) nil t 2)
    (forward-char)
    (insert (make-string 3 ?\n))
    (forward-line -1)
    (delete-blank-lines)))

;;; functions

(defun qz-list-from-region (start end &optional char)
  "Get list from region."
  (unless (characterp char)
    (setq char ?\n))
  (split-string
   (buffer-substring-no-properties start end)
   (make-string 1 char) t))

(defun qz-table-p (fields)
  "Make sure the first is id_field and last is field_status."
  (and (string-match "^id_" (car fields))
       (string-match "_status$" (car (last fields)))))

(defun qz-table-name (table-or-field)
  "Get table name from fields or id_field."
  (let ((field (cond ((listp table-or-field) (car table-or-field))
                     ((stringp table-or-field) table-or-field))))
    (replace-regexp-in-string "^id_" "" field)))

(defun qz-prevent-ambigu (field)
  "Return field or table.id_field."
  (if (string-match "^id_" field)
      (let ((table (qz-table-name field)))
        (format "%s.%s" table field))
    field))

(defun qz-controller-for-web-service (name)
  "Create controller for web service"
  (concat
   (qz-line 0 1 "<?php")
   (qz-line 0 0 "defined('BASEPATH') OR ")
   (qz-line 0 2 "exit('No direct script access allowed');")
   (qz-line 0 0 (format "class %s " name))
   (qz-line 0 1 "extends CI_Controller")
   (qz-line 0 1 "{")
   (qz-line 1 1 "public function __construct() {")
   (qz-line 2 1 "parent::__construct();")
   (qz-line 1 2 "}")
   (qz-line 0 0 "}")))

(defun qz-function-from-fields (fields name &optional controller)
  "Create function from table fields."
  (if (qz-table-p fields)
      (progn
        (unless controller
          (setq controller "Android"))
        (qz-open-continue-buffer (concat controller ".php"))
        (when (equal 1 (point-at-bol))
          (insert (qz-controller-for-web-service controller))
          (forward-line -1))
        (when (fboundp 'web-mode) (web-mode))
        (insert
         (qz-line 0 1 "")
         (qz-line 1 1 (format "public function %s() {" name))
         (qz-line 2 1 "$data = array();")
         (qz-line 2 2 "$data['result'] = 'false';")

         ;; content get / set or if-get
         (qz-function-contents fields 2)

         (qz-line 0 1 "")
         (qz-line 2 1 "header('Content-Type: application/json');")
         (qz-line 2 1 "echo json_encode($data);")
         (qz-line 1 0 "}")))
    (print "Selected region not well formatted")))

(defun qz-function-contents (fields &optional ntab neol)
  "Choose content type first before execution."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let* ((choise '(("get" . "get")
                   ("set" . "set")
                   ("if-get" . "if-get")))
         (choosen (completing-read
                   "Function Mode (get): " ; prompt
                   choise                  ; collection
                   nil                     ; predicate
                   t                       ; require-match
                   nil                     ; initial-input
                   nil                     ; hist
                   "get"                   ; def
                   )))
    (cond ((equal choosen "get")
           (qz-function-content-get fields ntab neol))
          ((equal choosen "set")
           (qz-function-content-set fields ntab neol))
          ((equal choosen "if-get")
           (qz-function-content-if-get fields ntab neol)))))

(defun qz-function-content-get (fields &optional ntab neol)
  "Content for request data."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (concat
   (qz-get-content (butlast fields) ntab neol)
   (qz-line 0 1 "")
   (qz-get-query-content fields ntab neol)))

(defun qz-get-content (fields &optional ntab neol)
  "Select option to get."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((one (car fields))
        (body (cdr fields))
        (container ""))
    (setq
     container
     (qz-line ntab 1 (format "$select  = \"%s, \";" one)))
    (when (> (length body) 1)
      (mapc
       (lambda (field)
         (setq
          container
          (concat
           container
           (qz-line ntab 1 (format "$select .= \"%s, \";"
                                   (qz-prevent-ambigu field))))))
       (butlast body)))
    (concat
     container
     (qz-line ntab 1 (format "$select .= \"%s \";" (car (reverse body)))))))

(defun qz-get-join-fields (fields)
  "Filter fields for join."
  (let ((names (rest (butlast fields))))
    (delq
     nil
     (mapcar
      (lambda (field)
        (when (string-match "^id_" field)
          field))
      names))))

(defun qz-get-filtered-fields (fields &optional string)
  "Get fields after filtered."
  (unless string
    (setq string "fields"))
  (completing-read-multiple
   (concat
    "Optional " string " with comma (nil): ") ; prompt
   (butlast fields)                           ; collection
   nil                                        ; predicate
   t                                          ; require-match
   nil                                        ; initial-input
   nil                                        ; hist
   nil                                        ; def
   ))

(defun qz-get-query-content (fields &optional ntab neol wheres)
  "Query to get data"
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((table (qz-table-name fields))
        (state (car (reverse fields)))
        (joins (qz-get-join-fields fields)))
    (concat
     (qz-line ntab 1 "$this->db->select($select);")
     (when joins
       (let ((container ""))
         (mapc
          (lambda (id)
            (setq
             container
             (concat
              container
              (qz-line ntab 0 "$this->db->join(")
              (qz-line 0 0 (format "'%s', " (qz-table-name id)))
              (qz-line 0 0 (format "'%s.%s=" (qz-table-name id) id))
              (qz-line 0 1 (format "%s.%s');" table id)))))
          joins)
         container))
     (qz-line ntab 1 (format "$this->db->where('%s', '1');" state))
     (when wheres
       (let ((container ""))
         (mapc
          (lambda (field)
            (setq
             container
             (concat
              container
              (qz-line ntab 0 "$this->db->where")
              (qz-line 0 0 (format "('%s', " (qz-prevent-ambigu field)))
              (qz-line 0 0 "$this->input->post")
              (qz-line 0 1 (format "('f_%s'));" field)))))
          wheres)
         container))
     (qz-line ntab 0 (format "$q_%s = $this->" table))
     (qz-line 0 1 (format "db->get('%s');" table))
     (qz-line ntab 1 (format "if ($q_%s->num_rows() > 0) {" table))
     (qz-line (+ ntab 1) 0 "$data['data'] = ")
     (qz-line 0 1 (format "$q_%s->result();" table))
     (qz-line (+ ntab 1) 1 "$data['result'] = 'true';")
     (qz-line ntab 1 "}"))))

(defun qz-function-content-set (fields &optional ntab neol)
  "Content for input data."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (concat
   (qz-fields-validation (cdr (butlast fields)) ntab neol)
   (qz-line 0 1 "")
   (qz-if-post-open (cdr (butlast fields)) ntab neol)
   (qz-if-post-content (butlast fields) (+ ntab 2) neol)
   (qz-line 0 1 "")
   (qz-if-post-insert-update fields (+ ntab 2) neol)
   (qz-line (+ ntab 2) 1 "$data['data'] = \"Sukses\";")
   (qz-if-post-close ntab neol t)))

(defun qz-function-content-if-get (fields &optional ntab neol)
  "Content with conditional term to access."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((valids (qz-get-filtered-fields fields "validations")))
    (concat
     (qz-fields-validation valids ntab neol)
     (qz-line 0 1 "")
     (qz-if-post-open (butlast fields) ntab neol)
     (qz-get-content (butlast fields) (+ ntab 2) neol)
     (qz-line 0 1 "")
     (qz-get-query-content fields (+ ntab 2) neol
                           (qz-get-filtered-fields
                            fields "query conditions"))
     (qz-if-post-close ntab neol))))

(defun qz-if-post-insert-update (fields &optional ntab neol)
  "Input or update database."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((table (qz-table-name fields))
        (field (car fields)))
    (concat
     (qz-line ntab 0 (format "if ($%s = " field))
     (qz-line 0 0 "$this->input->post")
     (qz-line 0 1 (format "('f_%s')) {" field))
     (qz-line (+ ntab 1) 0 "$this->db->where")
     (qz-line 0 1 (format "('%s', $%s);" field field))
     (qz-line (+ ntab 1) 0 "$this->db->update")
     (qz-line 0 1 (format "('%s', $data_%s);" table table))
     (qz-line ntab 1 "} else {")
     (qz-line (+ ntab 1) 0 "$this->db->insert")
     (qz-line 0 1 (format "('%s', $data_%s);" table table))
     (qz-line ntab 1 "}"))))

(defun qz-if-post-open (fields &optional ntab neol)
  "Content condition for input or conditional access."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let* ((choice fields)
         (choosen (completing-read
                   "Condition field: "   ; prompt
                   choice                ; collection
                   nil                   ; predicate
                   t                     ; require-match
                   nil                   ; initial-input
                   nil                   ; hist
                   (car choice)          ; def
                   )))
    (concat
     (qz-line ntab 0 "if ($this->input->post")
     (qz-line 0 1 (format "('f_%s')) {" choosen))
     (qz-line (+ ntab 1) 0 "if ($this->form_validation")
     (qz-line 0 1 "->run() !== FALSE) {")
     (qz-line 0 neol ""))))

(defun qz-if-post-content (fields &optional ntab neol)
  "Get data from input post."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((posts "")
        (table (qz-table-name fields)))
    (mapc
     (lambda (field)
       (setq
        posts
        (concat
         posts
         (qz-line ntab 0 (format "$data_%s['%s'] = " table field))
         (qz-line 0 1 (format "$this->input->post('f_%s');" field)))))
     (cdr fields))
    (qz-line 0 neol posts)))

(defun qz-if-post-close (&optional ntab neol result)
  "Close condition content."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (concat
   (when result
     (qz-line (+ ntab 2) 1 "$data['result'] = 'true';"))
   (qz-line (+ ntab 1) 1 "} else {")
   (qz-line (+ ntab 2) 1 "$data['data'] = \"Gagal validasi\";")
   (qz-line (+ ntab 1) 1 "}")
   (qz-line ntab 1 "}")
   (qz-line 0 neol "")))

(defun qz-fields-validation (fields &optional ntab neol)
  "Get string validation."
  (unless ntab
    (setq ntab 0))
  (unless neol
    (setq neol 0))
  (let ((validations ""))
    (unless fields
      (setq
       validations
       (qz-line ntab 1 "// need validation.")))
    (mapc
     (lambda (field)
       (setq
        validations
        (concat
         validations
         (qz-line ntab 0 "$this->form_validation->set_rules(")
         (qz-line 0 0 (format "'f_%s', '%s', " field field))
         (qz-line 0 1 "'required');"))))
     fields)
    (qz-line 0 neol validations)))

;;; commands

(defun qz-create-codeigniter-web-service ()
  "Start creating file buffer for services."
  (interactive)
  (let ((controller
         (read-from-minibuffer
          "Controller name: "
          "Android")))
    (qz-open-clear-buffer (concat controller ".php"))
    (insert (qz-controller-for-web-service controller))
    (forward-line -1)
    (when (fboundp 'web-mode) (web-mode))))

(defun qz-create-codeigniter-web-service-function ()
  "Create function from region."
  (interactive)
  (if (use-region-p)
	  (let* ((fields (qz-list-from-region
                      (region-beginning)
                      (region-end)))
             (controller (read-from-minibuffer
                          "Controller name: "
                          "Android"))
             (name (read-from-minibuffer
                    "Function name: "
                    (qz-table-name fields))))
        (qz-function-from-fields fields name controller))
	(print "No region selected")))

;; qz-ciws.el ends here
