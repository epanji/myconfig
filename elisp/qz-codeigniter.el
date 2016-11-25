;; Copyright (C) 2016  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Version: 0.1.5
;; Created: 28 September 2016
;; Keywords: codeigniter ci qzuma

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

(defun qz-open-clear-buffer (name)
  "Open buffer with specific name."
  (switch-to-buffer-other-window name)
  (erase-buffer))

(defun qz-list-from-region (start end)
  "Get list from region."
  (split-string (buffer-substring start end) "\n" t))

(defun qz-table-p (list-field)
  "Make sure the first is id_field and last is field_status."
  (if (and (string-match "^id_" (car list-field))
		   (string-match "_status$" (car (reverse list-field))))
	  t
	nil))

(defun qz-class-open (name)
  "Tag php plus class ci controller."
  (insert "<?php\n"
		  "defined('BASEPATH') OR "
		  "exit('No direct script access allowed');\n\n"
		  (format "class %s extends CI_Controller\n{\n" (capitalize name))))

(defun qz-name-controller (name)
  "Get formatted file name for controller."
  (concat (capitalize name) ".php"))

(defun qz-name-view (name sub)
  "Get formatted file name for view."
  (format "%s_%s.php" (downcase name) (downcase sub)))

(defun qz-label (field)
  "Replace underscore with space and capitalize it also remove 'Id '."
  (replace-regexp-in-string
   "Id " "" (capitalize (replace-regexp-in-string "_" " " field))))

(defun qz-name-table-from-id (id-field)
  "Get table name from id-field."
  (downcase (replace-regexp-in-string "^id_" "" id-field)))

(defun qz-join-table-if-exists (list-field tab)
  "Add join table on condition for selected region if needed."
  (let ((fields (cdr list-field))
		(table (qz-name-table-from-id (car list-field)))
		list-join)
	(while fields
	  (if (string-match "^id_" (car fields))
		  (push (car fields) list-join))
	  (setq fields (cdr fields)))
	(if list-join
		(setq fields (reverse list-join)))
	(while fields
	  (insert tab)
	  (insert (format "$this->db->join('%s', '%s.%s=%s.%s');\n"
					  (qz-name-table-from-id (car fields))
					  (qz-name-table-from-id (car fields))
					  (car fields)
					  table
					  (car fields)))
	  (setq fields (cdr fields)))))

(defun qz-resource-if-exists (list-field tab)
  "Add resource from publicmodel if needed."
  (let ((fields (cdr list-field))
		(table (qz-name-table-from-id (car list-field)))
		list-id)
	(while fields
	  (if (string-match "^id_" (car fields))
		  (push (car fields) list-id))
	  (setq fields (cdr fields)))
	(if list-id
		(setq fields (reverse list-id)))
	(while fields
	  (insert tab)
	  (insert (format "$data['%sArray'] = $this->p->%sArray();\n"
					  (qz-name-table-from-id (car fields))
					  (qz-name-table-from-id (car fields))))
	  (setq fields (cdr fields)))
	(if list-id
		(insert "\n"))))

(defun qz-input-post-file-if-exists (list-field tab)
  "Add file post if needed."
  (let ((fields list-field)
		(table (qz-name-table-from-id (car list-field)))
	    list-file)
	(while fields
	  (if (or (string-match "_file$" (car fields))
			  (string-match "_video$" (car fields))
			  (string-match "_clip$" (car fields))
			  (string-match "_movie$" (car fields))
			  (string-match "_audio$" (car fields))
			  (string-match "_sound$" (car fields))
			  (string-match "_song$" (car fields))
			  (string-match "_music$" (car fields))
			  (string-match "_suara$" (car fields))
			  (string-match "_lagu$" (car fields))
			  (string-match "_musik$" (car fields))
			  (string-match "_icon$" (car fields))
			  (string-match "_logo$" (car fields))
			  (string-match "_image$" (car fields))
			  (string-match "_photo$" (car fields))
			  (string-match "_picture$" (car fields))
			  (string-match "_foto$" (car fields))
			  (string-match "_gambar$" (car fields)))
		  (push (car fields) list-file))
	  (setq fields (cdr fields)))
	(if list-file
		(setq fields (reverse list-file)))
	(while fields
	  (insert tab)
	  (insert (format "if (isset($_FILES['f_%s']['name']) && " (car fields))
	  		  (format "$_FILES['f_%s']['name'] != '') {\n" (car fields)))
	  (insert tab)
	  (insert (format "\t$files = $_FILES['f_%s'];\n" (car fields)))
	  (insert tab)
	  (insert "\t$namafile = md5(date('Ymdhis'));\n")
	  (insert tab)
	  (insert "\tmove_uploaded_file($files['tmp_name'],\""
	  		  (format "./uploads/%s/\"" table)
	  		  ".$namafile.'_'.$files['name']);\n")
	  (insert tab)
	  (insert (format "\t$data_%s['%s'] = " table (car fields))
	  		  "$namafile.'_'.$files['name'];\n")
	  (insert tab)
	  (insert "}\n")
	  (setq fields (cdr fields)))
	(if list-file
		(insert "\n"))))

(defun qz-view-open (name sub)
  "Default opening file view."
  (progn (insert "<?php include('inc_admin_atas.php');?>\n\n")
		 (qz-title name sub)
		 (insert "<?php include('inc_admin_pesan.php');?>\n\n")))

(defun qz-view-close ()
  "Default closing file view."
  (insert "<?php include('inc_admin_bawah.php');?>"))

(defun qz-title (name sub)
  "Generate title from view sub name."
  (cond ((string-equal sub "index")
		 (insert (format "<h3>Form Data %s</h3>\n\n" (qz-label name))))
		((string-equal sub "data")
		 (insert (format "<h3>Daftar Data %s</h3>\n\n" (qz-label name))))
		((string-equal sub "detail")
		 (insert (format "<h3>Detail Data %s</h3>\n\n" (qz-label name))))
		((string-equal sub "password")
		 (insert "<h3>Form Password</h3>\n\n"))))

(defun qz-view-table (list-field controller)
  "Generate table from fields."
  (let ((fields list-field)
		(id-field (car list-field))
		(table (qz-name-table-from-id (car list-field)))
		(span 4))
	(insert "<?php echo $this->pagination->create_links();?>\n\n")
	(insert "<table class=\"table table-striped\">\n\n"
			"\t<thead>\n"
			"\t\t<tr>\n"
			"\t\t\t<td width=\"10\">No</td>\n")
	(while fields
	  (if (not (or (string-match "^id_" (car fields))
				   (string-match "_status$" (car fields))))
		  (progn (insert (format "\t\t\t<td>%s</td>\n"
								 (qz-label (car fields))))
				 (setq span (+ 1 span))))
	  (setq fields (cdr fields)))
	(setq fields list-field)
	(insert "\t\t\t<td width=\"20%\" colspan=\"3\">Aksi</td>\n"
			"\t\t</tr>\n"
			"\t</thead>\n\n")
	(insert (format "\t<?php if ($%s->num_rows() == 0) { ?>\n"
				    table)
			"\t\t<tbody>\n"
			"\t\t\t<tr>\n"
			(format "\t\t\t\t<td colspan=\"%s\">" span)
			"Maaf, belum ada data.</td>\n"
			"\t\t\t</tr>\n"
			"\t\t</tbody>\n"
			"\t<?php } else { ?>\n"
			(format "\t\t<?php foreach($%s->result() as $row) { $no++;?>\n"
				    table))
	(insert "\t\t\t<tr>\n")
	(insert "\t\t\t\t<td><?php echo $no;?></td>\n")
	(while fields
	  (if (not (or (string-match "^id_" (car fields))
				   (string-match "_status$" (car fields))))
		  (cond ((or (string-match "_icon$" (car fields))
					 (string-match "_logo$" (car fields))
					 (string-match "_image$" (car fields))
					 (string-match "_photo$" (car fields))
					 (string-match "_picture$" (car fields))
					 (string-match "_foto$" (car fields))
					 (string-match "_gambar$" (car fields)))
				 (insert "\t\t\t\t<td><img width=\"100\" alt=\"-\" src=\""
						 "<?php echo base_url();?>"
						 (format "uploads/%s/" table)
						 (format "<?php echo $row->%s;?>\" /></td>\n"
								 (car fields))))
				((or (string-match "_audio$" (car fields))
					 (string-match "_sound$" (car fields))
					 (string-match "_song$" (car fields))
					 (string-match "_music$" (car fields))
					 (string-match "_suara$" (car fields))
					 (string-match "_lagu$" (car fields))
					 (string-match "_musik$" (car fields)))
				 (insert "\t\t\t\t<td>\n"
						 "\t\t\t\t\t<audio controls>\n"
						 "\t\t\t\t\t\t<source src=\"<?php echo "
						 (format "base_url();?>uploads/%s/" table)
						 (format "<?php echo $row->%s;?>\" " (car fields))
						 "type=\"audio/ogg\">\n"
						 "\t\t\t\t\t\t<source src=\"<?php echo "
						 (format "base_url();?>uploads/%s/" table)
						 (format "<?php echo $row->%s;?>\" " (car fields))
						 "type=\"audio/mpeg\">\n"
						 "\t\t\t\t\t\t<source src=\"<?php echo "
						 (format "base_url();?>uploads/%s/" table)
						 (format "<?php echo $row->%s;?>\" " (car fields))
						 "type=\"audio/mp4\">\n"
						 "\t\t\t\t\t\tYour browser does not support the "
						 "audio element.\n"
						 "\t\t\t\t\t</audio>\n"
						 "\t\t\t\t</td>\n"))
				((or (string-match "_video$" (car fields))
					 (string-match "_clip$" (car fields))
					 (string-match "_movie$" (car fields)))
				 (insert "\t\t\t\t<td>\n"
						 "\t\t\t\t\t<video width=\"320\" height=\"240\" "
						 "controls>\n"
						 "\t\t\t\t\t\t<source src=\"<?php echo "
						 (format "base_url();?>uploads/%s/" table)
						 (format "<?php echo $row->%s;?>\" " (car fields))
						 "type=\"video/mp4\">\n"
						 "\t\t\t\t\t\t<source src=\"<?php echo "
						 (format "base_url();?>uploads/%s/" table)
						 (format "<?php echo $row->%s;?>\" " (car fields))
						 "type=\"video/ogg\">\n"
						 "\t\t\t\t\t\tYour browser does not support the "
						 "video tag.\n"
						 "\t\t\t\t\t</video>\n"
						 "\t\t\t\t</td>\n"))
				(t (insert "\t\t\t\t<td><?php echo "
						   (format "$row->%s;?></td>\n"
								   (car fields))))))
	  (setq fields (cdr fields)))
	(insert "\n")
	(insert "\t\t\t\t<td><a href=\"<?php echo base_url();?>"
			(format "%s/index/ubahdata/<?php echo $row->%s;?>\" \n"
				    controller id-field)
			"\t\t\t\t\tclass=\"btn btn-primary\" >"
			"<span class=\"glyphicon glyphicon-edit\">"
			"</span> Perbarui</a></td>\n")
	(insert "\t\t\t\t<td><a href=\"<?php echo base_url();?>"
			(format "%s/detail/<?php echo $row->%s;?>\" \n"
				    controller id-field)
			"\t\t\t\t\tclass=\"btn btn-info\" >"
			"<span class=\"glyphicon glyphicon-list-alt\">"
			"</span> Detail</a></td>\n")
	(insert "\t\t\t\t<td><a href=\"<?php echo base_url();?>"
			(format "%s/updatestatus/<?php echo $row->%s;?>/0\" \n"
				    controller id-field)
			"\t\t\t\t\tonclick=\"return confirm"
			"('Anda yakin ingin menghapus data ini?')\" \n"
			"\t\t\t\t\tclass=\"btn btn-danger\" >"
			"<span class=\"glyphicon glyphicon-trash\">"
			"</span> Hapus</a></td>\n")
	(insert "\t\t\t</tr>\n")
	(insert "\t\t<?php } ?>\n"
			"\t<?php } ?>\n\n"
			"</table>\n\n"
			"<a href=\"<?php echo base_url()?>"
			(format "%s\" " controller)
			"\n\tclass=\"btn btn-default\">"
			(format "Tambah data %s</a>\n\n" table))))

(defun qz-view-table-detail (list-field controller)
  "Generate detail from fields."
  (let ((fields list-field)
		(id-field (car list-field))
		(table (qz-name-table-from-id (car list-field))))
	(insert (format "<?php $row = $%s->row();?>\n\n" table)
			"<table class=\"table table-striped\">\n\n")
	(while fields
	  (if (not (or (string-match "^id_" (car fields))
				   (string-match "_status$" (car fields))))
		  (progn (insert "\t<tr>\n"
						 (format "\t\t<td width=\"20%%\">%s</td>\n"
								 (qz-label (car fields)))
						 "\t\t<td width=\"10\">:</td>\n")
				 (cond ((or (string-match "_icon$" (car fields))
							(string-match "_logo$" (car fields))
							(string-match "_image$" (car fields))
							(string-match "_photo$" (car fields))
							(string-match "_picture$" (car fields))
							(string-match "_foto$" (car fields))
							(string-match "_gambar$" (car fields)))
						(insert "\t\t<td><img width=\"300\" alt=\"-\" \n"
								"\t\t\tsrc=\"<?php echo base_url();?>"
								(format "uploads/%s/" table)
								(format "<?php echo $row->%s;?>\" "
										(car fields))
								"/></td>\n"))
					   ((or (string-match "_audio$" (car fields))
							(string-match "_sound$" (car fields))
							(string-match "_song$" (car fields))
							(string-match "_music$" (car fields))
							(string-match "_suara$" (car fields))
							(string-match "_lagu$" (car fields))
							(string-match "_musik$" (car fields)))
						(insert "\t\t<td>\n"
								"\t\t\t<audio controls>\n"
								"\t\t\t\t<source src=\"<?php echo "
								(format "base_url();?>uploads/%s/" table)
								(format "<?php echo $row->%s;?>\" "
										(car fields))
								"type=\"audio/ogg\">\n"
								"\t\t\t\t<source src=\"<?php echo "
								(format "base_url();?>uploads/%s/" table)
								(format "<?php echo $row->%s;?>\" "
										(car fields))
								"type=\"audio/mpeg\">\n"
								"\t\t\t\t<source src=\"<?php echo "
								(format "base_url();?>uploads/%s/" table)
								(format "<?php echo $row->%s;?>\" "
										(car fields))
								"type=\"audio/mp4\">\n"
								"\t\t\t\tYour browser does not support the "
								"audio element.\n"
								"\t\t\t</audio>\n"
								"\t\t</td>\n"))
					   ((or (string-match "_video$" (car fields))
							(string-match "_clip$" (car fields))
							(string-match "_movie$" (car fields)))
						(insert "\t\t<td>\n"
								"\t\t\t<video width=\"320\" height=\"240\" "
								"controls>\n"
								"\t\t\t\t<source src=\"<?php echo "
								(format "base_url();?>uploads/%s/" table)
								(format "<?php echo $row->%s;?>\" "
										(car fields))
								"type=\"video/mp4\">\n"
								"\t\t\t\t<source src=\"<?php echo "
								(format "base_url();?>uploads/%s/" table)
								(format "<?php echo $row->%s;?>\" "
										(car fields))
								"type=\"video/ogg\">\n"
								"\t\t\t\tYour browser does not support the "
								"video tag.\n"
								"\t\t\t</video>\n"
								"\t\t</td>\n"))
					   (t (insert "\t\t<td><?php echo "
								  (format "$row->%s;?></td>\n"
										  (car fields)))))
				 (insert "\t</tr>\n")))
	  (setq fields (cdr fields)))
	(insert "\n")
	(insert "</table>\n\n")
	(insert "<a href=\"<?php echo base_url();?>"
			(format "%s/data\" " controller)
			"\n\tclass=\"btn btn-default\">"
			(format "Kembali ke data %s</a>\n\n" table))))

(defun qz-form-multi-if-needed (list-field)
  "Check if multipart needed."
  (let ((fields list-field)
		multi)
	(setq multi nil)
	(while fields
	  (if (or (string-match "_file$" (car fields))
			  (string-match "_video$" (car fields))
			  (string-match "_clip$" (car fields))
			  (string-match "_movie$" (car fields))
			  (string-match "_audio$" (car fields))
			  (string-match "_sound$" (car fields))
			  (string-match "_song$" (car fields))
			  (string-match "_music$" (car fields))
			  (string-match "_suara$" (car fields))
			  (string-match "_lagu$" (car fields))
			  (string-match "_musik$" (car fields))
			  (string-match "_icon$" (car fields))
			  (string-match "_logo$" (car fields))
			  (string-match "_image$" (car fields))
			  (string-match "_photo$" (car fields))
			  (string-match "_picture$" (car fields))
			  (string-match "_foto$" (car fields))
			  (string-match "_gambar$" (car fields)))
		  (setq multi t))
	  (setq fields (cdr fields)))
	(if multi
		(setq multi (concat " \n\taccept-charset=\"utf-8\" "
							"enctype=\"multipart/form-data\""))
	  (setq multi ""))))

(defun qz-form-dropdown (field)
  "Codeigniter form dropdown."
  (insert "\t<div class=\"form-group\">\n"
		  "\t\t<label class=\"control-label col-sm-2\">"
		  (format "%s</label>\n" (qz-label field))
		  "\t\t<div class=\"col-sm-10\">\n"
		  "\t\t\t<?php echo form_dropdown"
		  (format "('f_%s', $%sArray, @$row->%s, "
				  field (qz-name-table-from-id field) field)
		  "'class=\"form-control\"');?>\n"
		  (format "\t\t\t<?php echo form_error('f_%s');?>\n" field)
		  "\t\t</div>\n"
		  "\t</div>\n\n"))

(defun qz-form-textarea (field)
  "Html textarea."
  (insert "\t<div class=\"form-group\">\n"
		  "\t\t<label class=\"control-label col-sm-2\">"
		  (format "%s</label>\n" (qz-label field))
		  "\t\t<div class=\"col-sm-10\">\n"
		  "\t\t\t<textarea class=\"form-control ckeditor\" "
		  (format "rows=\"10\" \n\t\t\t\tname=\"f_%s\">" field)
		  (format "<?php echo set_value('f_%s', @$row->%s);"
				  field field)
		  "?></textarea>\n"
		  (format "\t\t\t<?php echo form_error('f_%s');?>\n" field)
		  "\t\t</div>\n"
		  "\t</div>\n\n"))

(defun qz-form-input (field)
  "Html input file text password."
  (let ((type "text")
		(value (format
				"<?php echo set_value('f_%s', @$row->%s);?>"
				field field))
		(class ""))
	(insert "\t<div class=\"form-group\">\n"
			"\t\t<label class=\"control-label col-sm-2\">"
			(format "%s</label>\n" (qz-label field))
			"\t\t<div class=\"col-sm-10\">\n")
	(cond ((string-match "_password$" field)
		   (progn (setq type "password")
				  (setq value "")))
		  ((or (string-match "_file$" field)
			   (string-match "_video$" field)
			   (string-match "_clip$" field)
			   (string-match "_movie$" field)
			   (string-match "_audio$" field)
			   (string-match "_sound$" field)
			   (string-match "_song$" field)
			   (string-match "_music$" field)
			   (string-match "_suara$" field)
			   (string-match "_lagu$" field)
			   (string-match "_musik$" field)
			   (string-match "_icon$" field)
			   (string-match "_logo$" field)
			   (string-match "_image$" field)
			   (string-match "_photo$" field)
			   (string-match "_picture$" field)
			   (string-match "_foto$" field)
			   (string-match "_gambar$" field))
		   (progn (setq type "file")
				  (setq value "")))
		  ((or (string-match "_tanggal$" field)
			   (string-match "_tgl$" field)
			   (string-match "_date$" field)
			   (string-match "_datetime$" field))
		   (progn (setq type "text")
				  (setq class " datepicker")))
		  ((or (string-match "_waktu$" field)
			   (string-match "_pukul$" field)
			   (string-match "_time$" field))
		   (progn (setq type "text")
				  (setq class " timepicker")))
		  (t (setq type "text")))
	(insert (format "\t\t\t<input class=\"form-control%s\" " class)
			(format "type=\"%s\" \n\t\t\t\tname=\"f_%s\" value=\"%s\"/>\n"
					type field value))
	(insert (format "\t\t\t<?php echo form_error('f_%s');?>\n" field)
			"\t\t</div>\n"
			"\t</div>\n\n")))

(defun qz-form-field-p (string field)
  "Check available form from field."
  (cond ((string-equal string "dropdown")
  		 (if (string-match "^id_" field)
  			 t
  		   nil))
  		((string-equal string "textarea")
  		 (if (or (string-match "_isi$" field)
  				 (string-match "_konten$" field)
  				 (string-match "_deskripsi$" field)
  				 (string-match "_keterangan" field)
  				 (string-match "_content" field)
  				 (string-match "_description" field))
  			 t
  		   nil))
  		(t nil)))

(defun qz-view-form (list-field controller)
  "Generate html form from fields."
  (let ((fields (cdr list-field))
		(id-field (car list-field))
		(table (qz-name-table-from-id (car list-field))))
	(insert "<form class=\"form-horizontal\" "
			(format "name=\"form_%s\" " table)
			"method=\"post\" action=\"\""
			(format "%s>\n\n" (qz-form-multi-if-needed list-field))
			(format "\t<?php if (isset($record_%s)) { ?>\n" table)
			(format "\t\t<?php $row = $record_%s->row();?>\n" table)
			(format "\t\t<input type=\"hidden\" name=\"f_%s\" " id-field)
			(format "value=\"<?php echo @$row->%s;?>\"/>\n" id-field)
			"\t<?php } ?>\n\n")
	(while fields
	  (if (not (string-match "_status$" (car fields)))
		  (cond ((qz-form-field-p "dropdown" (car fields))
		  		 (qz-form-dropdown (car fields)))
		  		((qz-form-field-p "textarea" (car fields))
		  		 (qz-form-textarea (car fields)))
		  		(t (qz-form-input (car fields)))))
	  (setq fields (cdr fields)))
	(insert "\t<div class=\"form-group\">\n"
			"\t\t<label class=\"control-label col-sm-2\">&nbsp;</label>\n"
			"\t\t<div class=\"col-sm-10\">\n"
			"\t\t\t<input class=\"btn btn-primary\" type=\"submit\" "
			"name=\"b_simpan\" \n\t\t\t\tvalue=\"<?php echo "
			"($aksi == 'tambahdata' ? 'Simpan' : 'Perbarui');?>\"/>\n"
			"\t\t</div>\n"
			"\t</div>\n\n")
	(insert "</form>\n\n")
	(insert "<a href=\"<?php echo base_url();?>"
			(format "%s/data\" " controller)
			"\n\tclass=\"btn btn-default\">"
			(format "Kembali ke data %s</a>\n\n" table))))

(defun qz-n-tab (ntab)
  "Multiply string tab by number."
  (if (and (numberp ntab) (> ntab 0))
	  (make-string ntab ?\t) ""))

(defun qz-n-eol (neol)
  "Multiply string end of line by number."
  (if (and (numberp neol) (> neol 0))
	  (make-string neol ?\n) ""))

(defun qz-insert (ntab neol &rest body)
  "Simplify insert line with number tab and eol."
  (let ((stab (qz-n-tab ntab))
		(seol (qz-n-eol neol)))
	(insert stab)
	(mapc 'insert body)
	(insert seol)))

(defun qz-login-actor-credential (list-field)
  "Get list actor and credential from list-field."
  (let ((fields list-field)
		(actor nil)
		(credential nil))
	(while fields
	  (if (or (string-match "_username$" (car fields))
			  (string-match "_email$" (car fields))
			  (string-match "_nip$" (car fields))
			  (string-match "_nim$" (car fields)))
		  (setq actor (car fields)))
	  (if (or (string-match "_password$" (car fields))
			  (string-match "_passwd$" (car fields)))
		  (setq credential (car fields)))
	  (setq fields (cdr fields)))
	(if (and actor credential)
		(list actor credential)
	  nil)))

;;
;; function construct
;;
(defun qz-function-construct ()
  "Define php constructor."
  (insert "\tpublic function __construct() {\n"
		  "\t\tparent::__construct();\n"
		  "\t\t$this->load->model('publicmodel', 'p');\n"
		  "\t}\n\n"))

;;
;; function index
;;
(defun qz-function-index (list-field controller)
  "Create function index for C U from crud."
  (insert "\tpublic function index($aksi='tambahdata', $id='') {\n"
		  "\t\t$data = array();\n"
		  "\t\t$data['aksi'] = $aksi;\n\n")
  (qz-resource-if-exists list-field "\t\t")
  (insert "\t\t$this->form_validation->set_error_delimiters"
		  "('<br /><span class=\"help-block error\">', "
		  "'</span>');\n")
  (mapc 'qz-rule-validation (cdr list-field))
  (insert "\n")
  (qz-condition-update list-field)
  (qz-condition-click list-field controller)
  (insert "\t\tif (isset($url)) {\n"
		  "\t\t\tredirect($url);\n"
		  "\t\t}\n\n"
		  (format "\t\t$this->load->view('%s_index', $data);\n" controller)
		  "\t}\n\n"))

(defun qz-condition-update (list-field)
  "Condition update in controller."
  (let ((table (qz-name-table-from-id (car list-field)))
		(id-field (car list-field))
		(field-status (car (reverse list-field))))
	(insert "\t\tif ($id != '') {\n")
	(insert "\t\t\t$this->form_validation->set_rules"
			(format "('f_%s', '%s', 'required');\n"
					id-field (qz-label id-field)))
	(qz-join-table-if-exists list-field "\t\t\t")
	(insert (format "\t\t\t$this->db->where('%s', '1');\n" field-status)
			(format "\t\t\t$this->db->where('%s', $id);\n" id-field)
			(format "\t\t\t$data['record_%s'] = $this->db->get('%s');\n"
					table table)
			"\t\t}\n\n")))

(defun qz-condition-click (list-field controller)
  "Condition where button clicked from form."
  (insert "\t\tif ($this->input->post('b_simpan')) {\n"
		  "\t\t\tif ($this->form_validation->run() !== FALSE) {\n")
  (qz-data-input-post list-field)
  (qz-input-post-file-if-exists list-field "\t\t\t\t")
  (qz-condition-insert-update list-field)
  (insert (format "\t\t\t\t$url = base_url().'%s/data';\n" controller)
		  "\t\t\t}\n\t\t}\n\n"))

(defun qz-condition-insert-update (list-field)
  "Create condition to save data, base on insert or update."
  (let ((id-field (car list-field))
		(table (qz-name-table-from-id (car list-field))))
	(insert (format "\t\t\t\tif ($%s = $this->input->post('f_%s')) {\n"
					id-field id-field)
			(format "\t\t\t\t\t$this->db->where('%s', $%s);\n"
					id-field id-field)
			(format "\t\t\t\t\t$this->db->update('%s', $data_%s);\n"
					table table)
			"\t\t\t\t\t$this->session->set_flashdata"
			"('pesan', 'Data berhasil di-Perbarui');\n";
			"\t\t\t\t} else {\n"
			(format "\t\t\t\t\t$this->db->insert('%s', $data_%s);\n"
					table table)
			"\t\t\t\t\t$this->session->set_flashdata"
			"('pesan', 'Data berhasil di-Simpan');\n";
			"\t\t\t\t}\n")))

(defun qz-data-input-post (list-field)
  "Change list-field to format array."
  (let ((fields (cdr list-field))
		(table (qz-name-table-from-id (car list-field))))
	(while fields
	  (if (or (string-match "_status$" (car fields))
			  (string-match "_file$" (car fields))
			  (string-match "_video$" (car fields))
			  (string-match "_clip$" (car fields))
			  (string-match "_movie$" (car fields))
			  (string-match "_audio$" (car fields))
			  (string-match "_sound$" (car fields))
			  (string-match "_song$" (car fields))
			  (string-match "_music$" (car fields))
			  (string-match "_suara$" (car fields))
			  (string-match "_lagu$" (car fields))
			  (string-match "_musik$" (car fields))
			  (string-match "_icon$" (car fields))
			  (string-match "_logo$" (car fields))
			  (string-match "_image$" (car fields))
			  (string-match "_photo$" (car fields))
			  (string-match "_picture$" (car fields))
			  (string-match "_foto$" (car fields))
			  (string-match "_gambar$" (car fields)))
		  nil
		(if (or (string-match "_password$" (car fields))
				(string-match "_passwd$" (car fields)))
			(insert (format "\t\t\t\tif ($%s = $this->input->post"
							(car fields))
					(format "('f_%s')) {\n" (car fields))
					(format "\t\t\t\t\t$data_%s['%s'] = "
							table (car fields))
					(format "md5($%s);\n" (car fields))
					"\t\t\t\t}\n" )
		  (insert "\t\t\t\t$data_"
				  (format "%s['%s'] = $this->input->post('f_%s');\n"
						  table (car fields) (car fields)))))
	  (setq fields (cdr fields)))
	(insert "\n")))

(defun qz-rule-validation (field)
  "Change field to ci rule validation."
  (let (condition)
	(if (or (string-match "_password$" field)
			(string-match "_passwd$" field))
		(setq condition "if ($id == '') ")
	  (setq condition ""))
	(if (or (string-match "_status$" field)
			(string-match "_file$" field)
			(string-match "_video$" field)
			(string-match "_clip$" field)
			(string-match "_movie$" field)
			(string-match "_audio$" field)
			(string-match "_sound$" field)
			(string-match "_song$" field)
			(string-match "_music$" field)
			(string-match "_suara$" field)
			(string-match "_lagu$" field)
			(string-match "_musik$" field)
			(string-match "_icon$" field)
			(string-match "_logo$" field)
			(string-match "_image$" field)
			(string-match "_photo$" field)
			(string-match "_picture$" field)
			(string-match "_foto$" field)
			(string-match "_gambar$" field))
		nil
	  (insert (format "\t\t%s$this->" condition)
			  "form_validation->set_rules"
			  (format "('f_%s', '%s', 'required');\n"
					  field (qz-label field))))))

;;
;; function update status
;;
(defun qz-function-updatestatus (list-field controller)
  "Create function update status."
  (let ((table (qz-name-table-from-id (car list-field)))
		(table-status (car (reverse list-field))))
	(insert "\tpublic function updatestatus($id, $status) {\n"
			(format "\t\t$data_%s['%s'] = $status;\n" table table-status)
			(format "\t\t$this->db->where('%s', $id);\n" (car list-field))
			(format "\t\t$this->db->update('%s', $data_%s);\n" table table)
			"\t\tswitch ($status) {\n"
			"\t\t\tcase 0:\n"
			"\t\t\t\t$pesan = \"Hapus\";\n"
			"\t\t\t\tbreak;\n"
			"\t\t}\n"
			"\t\t$this->session->set_flashdata"
			"('pesan', 'Data berhasil di-' . $pesan);\n"
			(format "\t\t$url = base_url() . '%s/data';\n" controller)
			"\t\tredirect($url);\n"
			"\t}\n\n")))

;;
;; function data
;;
(defun qz-function-data (list-field controller)
  "Create function data for R from crud."
  (let ((table (qz-name-table-from-id (car list-field)))
		(table-status (car (reverse list-field))))
	(insert "\tpublic function data($halaman=0) {\n"
			"\t\t$data = array();\n"
			"\t\t$data['no'] = $halaman;\n\n"
			"\t\t$datatiaphalaman = 30;\n")
	(qz-join-table-if-exists list-field "\t\t")
	(insert (format "\t\t$this->db->where('%s', '1');\n" table-status)
			"\t\t$config['base_url'] = "
			(format "base_url() . '%s/data/';\n" controller)
			"\t\t$config['total_rows'] = "
			(format "$this->db->count_all_results('%s');\n" table)
			"\t\t$config['per_page'] = $datatiaphalaman;\n"
			"\t\t$this->pagination->initialize($config);\n\n")
	(qz-join-table-if-exists list-field "\t\t")
	(insert (format "\t\t$this->db->where('%s', '1');\n"
					table-status)
			"\t\t$this->db->limit($datatiaphalaman, $halaman);\n"
			(format "\t\t$data['%s'] = $this->db->get('%s');\n"
					table table)
			"\n"
			(format "\t\t$this->load->view('%s_data', $data);\n"
					controller)
			"\t}\n\n")))

;;
;; function detail
;;
(defun qz-function-detail (list-field controller)
  "Create function detail."
  (let ((table (qz-name-table-from-id (car list-field))))
	(insert "\tpublic function detail($id) {\n"
			"\t\t$data = array();\n\n")
	(qz-join-table-if-exists list-field "\t\t")
	(insert (format "\t\t$this->db->where('%s', $id);\n" (car list-field))
			(format "\t\t$data['%s'] = $this->db->get('%s');\n\n"
					table table)
			(format "\t\t$this->load->view('%s_detail', $data);\n"
					controller)
			"\t}\n\n")))

;;
;; function login index
;;
(defun qz-login-function-index (list-field controller)
  "Create function index in login controller."
  (let ((actor-credential (qz-login-actor-credential list-field)))
	(qz-insert 1 1 "public function index() {")
	(qz-insert 2 2 "$data = array();")
	(qz-insert 2 1 "$this->form_validation->set_error_delimiters"
			   "('<span class=\"help-block error\">', "
			   "'</span>');")
	(if actor-credential
		(mapc 'qz-login-rule-validation actor-credential))
	(qz-insert 0 1 "")
	(qz-login-condition-click list-field controller)
	(qz-insert 2 1 (format "$this->load->view('%s_index');" controller))
	(qz-insert 1 2 "}")))

(defun qz-login-rule-validation (field)
  "Change field to ci rule validation for login."
  (qz-insert 2 1 "$this->form_validation->set_rules"
			 (format "('f_%s', '%s', 'required');"
					 field (qz-label
							(replace-regexp-in-string
							 "^[a-z]+_" "" field)))))

(defun qz-login-condition-click (list-field controller)
  "Condition where login button clicked from form."
  (qz-insert 2 1 "if ($this->input->post('b_login')) {")
  (qz-insert 3 1 "if ($this->form_validation->run() !== FALSE) {")
  (qz-insert 4 2 (format "$url = base_url().'%s/index';" controller))
  (let ((table (qz-name-table-from-id (car list-field)))
		(actor-credential (qz-login-actor-credential list-field)))
	(if actor-credential
		(progn
		  (qz-login-form-input actor-credential)
		  (qz-login-check-database controller table actor-credential))))
  (qz-insert 4 1 "redirect($url);")
  (qz-insert 3 1 "}")
  (qz-insert 2 2 "}"))

(defun qz-login-form-input (actor-credential)
  "Html input form login"
  (qz-insert
   4 1 (format "$%s = $this->input->post('f_%s');"
			   (elt actor-credential 0)
			   (elt actor-credential 0)))
  (qz-insert
   4 2 (format "$%s = md5($this->input->post('f_%s'));"
			   (elt actor-credential 1)
			   (elt actor-credential 1))))

(defun qz-login-check-database (controller table actor-credential)
  "Check user input credential with database."
  (qz-insert
   4 1
   (format "$this->db->where('%s_status', '1');" table))
  (qz-insert
   4 1 (format "$this->db->where('%s', $%s);"
			   (elt actor-credential 0)
			   (elt actor-credential 0)))
  (qz-insert
   4 1 (format "$this->db->where('%s', $%s);"
			   (elt actor-credential 1)
			   (elt actor-credential 1)))
  (qz-insert
   4 1 (format "$q_%s = $this->db->get('%s');"
			   table table))
  (qz-insert 4 1 (format "if ($q_%s->num_rows() == 1) {" table))
  (qz-insert 5 2 (format "$row = $q_%s->row();" table))
  (qz-insert 5 1 (format "unset($row->%s);" (elt actor-credential 1)))
  (qz-insert 5 2 (format "unset($row->%s_status);" table))
  (qz-insert 5 1 (format "$data['%s'] = $row;" table))
  (qz-insert 5 2 "$this->session->set_userdata($data);")
  (qz-insert 5 1 (format "$url = base_url().'%s/home';" controller))
  (qz-insert 4 1 "} else {")
  (qz-insert 5 1 "$this->session->set_flashdata('pesan', "
			 (format "'%s atau %s salah.');"
					 (capitalize
					  (replace-regexp-in-string
					   (format "%s_" table)
					   "" (elt actor-credential 0)))
					 (replace-regexp-in-string
					  (format "%s_" table)
					  "" (elt actor-credential 1))))
  (qz-insert 4 1 "}"))

;;
;; function login home
;;
(defun qz-login-function-home (controller)
  "Create function home in login controller."
  (qz-insert 1 1 "public function home() {")
  (qz-insert 2 1 (format "$this->load->view('%s_home');" controller))
  (qz-insert 1 2 "}"))

;;
;; function login password
;;
(defun qz-login-function-password (list-field controller)
  "Create function password in login controller."
  (let ((actor-credential (qz-login-actor-credential list-field))
		(table (qz-name-table-from-id (car list-field)))
		repassword)
	(qz-insert 1 1 "public function password() {")
	(qz-insert 2 1 (format "$s_%s = " table)
			   "$this->session->userdata"
			   (format "('%s');" table))
	(qz-insert 2 2 (format "$id_%s = " table)
			   (format "$s_%s->id_%s;" table table))
	(qz-insert 2 1 "$this->form_validation->set_error_delimiters"
			   "('<span class=\"help-block error\">', "
			   "'</span>');")
	(if actor-credential
		(progn
		  (qz-login-rule-validation (elt actor-credential 1))
		  (setq repassword
				(replace-regexp-in-string
				 "\\(_\\).*\\'"
				 "_re_"
				 (elt actor-credential 1) nil nil 1))
		  (qz-insert 2 1 "$this->form_validation->set_rules"
					 (format "('f_%s', 'Re %s', "
							 repassword
							 (qz-label
							  (replace-regexp-in-string
							   "^[a-z]+_re_" "" repassword)))
					 (format "'required|matches[f_%s]');"
							 (elt actor-credential 1)))
		  (qz-password-condition-click table actor-credential controller)))
	(qz-insert 2 1 (format "$this->load->view('%s_password');" controller))
	(qz-insert 1 2 "}")))

(defun qz-password-condition-click (table actor-credential controller)
  "Condition where password button clicked from form."
  (qz-insert 2 1 "if ($this->input->post('b_password')) {")
  (qz-insert 3 1 "if ($this->form_validation->run() !== FALSE) {")
  (qz-insert 4 2 (format "$newd['%s'] = md5($this->input->post('f_%s'));"
						 (elt actor-credential 1)
						 (elt actor-credential 1)))
  (qz-insert 4 1 (format "$this->db->where('id_%s', $id_%s);" table table))
  (qz-insert 4 2 (format "$this->db->update('%s', $newd);" table))
  (qz-insert 4 1 "$this->session->set_flashdata('pesan', "
			 "'Password berhasil di-perbarui');")
  (qz-insert 4 1 (format "$url = base_url().'%s/password';" controller))
  (qz-insert 4 1 "redirect($url);")
  (qz-insert 3 1 "}")
  (qz-insert 2 2 "}"))

;;
;; function login logout
;;
(defun qz-login-function-logout (list-field)
  "Create function logout in login controller."
  (let ((table (qz-name-table-from-id (car list-field))))
	(qz-insert 1 1 "public function logout() {")
	(qz-insert 2 1 "$this->session->unset_userdata"
			   (format "('%s');" table))
	(qz-insert 2 1 "$url = base_url();")
	(qz-insert 2 1 "redirect($url);")
	(qz-insert 1 1 "}")))

;;
;; commands from here
;;
(defun qz-create-controller ()
  "Create controller from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (qz-table-p list-field)
			(progn (qz-open-clear-buffer (qz-name-controller controller))
				   (qz-class-open controller)
				   (qz-function-construct)
				   (qz-function-index list-field controller)
				   (qz-function-updatestatus list-field controller)
				   (qz-function-data list-field controller)
				   (qz-function-detail list-field controller)
				   (insert "}"))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-create-publicmodel ()
  "Start creating file buffer for publicmodel.
Next, place cursor inside and command qz-create-model-function."
  (interactive)
  (progn (qz-open-clear-buffer "Publicmodel.php")
		 (insert "<?php\n"
				 "defined('BASEPATH') OR "
				 "exit('No direct script access allowed');\n\n"
				 "class Publicmodel extends CI_Model\n"
				 "{\n"
				 "\tpublic function __construct() {\n"
				 "\t\tparent::__construct();\n"
				 "\t}\n\n")
		 (insert "}")
		 (with-no-warnings (previous-line))))

(defun qz-create-model-function ()
  "Need to get ntab, id_field, field_label."
  (interactive)
  (let ((ntab (string-to-number (read-from-minibuffer
								 "Number of tab: " "1")))
		(id-field (read-from-minibuffer
				   "Table id name: " "id_table"))
		field-label)
	(setq field-label (read-from-minibuffer
					   "Table label name: "
					   (format "%s_name"
							   (qz-name-table-from-id
								id-field))))
	(if (and (> (length id-field) 0)
			 (> (length field-label) 0))
		(insert "\n"
				(make-string ntab ?\t)
				(format "public function %sArray() {\n"
						(qz-name-table-from-id id-field))
				(make-string ntab ?\t)
				"\t$data = array();\n\n"
				(make-string ntab ?\t)
				(format "\t$this->db->where('%s_status', '1');\n"
						(qz-name-table-from-id id-field))
				(make-string ntab ?\t)
				(format "\t$res = $this->db->get('%s');\n\n"
						(qz-name-table-from-id id-field))
				(make-string ntab ?\t)
				"\tforeach ($res->result() as $row) {\n"
				(make-string ntab ?\t)
				(format "\t\t$data[$row->%s] = $row->%s;\n"
						id-field field-label)
				(make-string ntab ?\t)
				"\t}\n"
				(make-string ntab ?\t)
				"\treturn $data;\n"
				(make-string ntab ?\t)
				"}\n")
	  (print "Something wrong with input"))))

(defun qz-create-view-index ()
  "Create view form for input and update."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (qz-table-p list-field)
			(progn (qz-open-clear-buffer (qz-name-view controller "index"))
				   (qz-view-open (car list-field) "index")
				   (qz-view-form list-field controller)
				   (qz-view-close))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-create-view-data ()
  "Create view data from controller."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (qz-table-p list-field)
			(progn (qz-open-clear-buffer (qz-name-view controller "data"))
				   (qz-view-open (car list-field) "data")
				   (qz-view-table list-field controller)
				   (qz-view-close))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-create-view-detail ()
  "Create view detail from controller."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (qz-table-p list-field)
			(progn (qz-open-clear-buffer (qz-name-view controller "detail"))
				   (qz-view-open (car list-field) "detail")
				   (qz-view-table-detail list-field controller)
				   (qz-view-close))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-create-login-controller ()
  "Create login controller from table fields in region."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (and (qz-table-p list-field)
				 (qz-login-actor-credential list-field))
			(progn (qz-open-clear-buffer (qz-name-controller controller))
				   (qz-class-open controller)
				   (qz-function-construct)
				   (qz-login-function-index list-field controller)
				   (qz-login-function-home controller)
				   (qz-login-function-password list-field controller)
				   (qz-login-function-logout list-field)
				   (insert "}"))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-create-login-view-home ()
  "Create login view home from controller."
  (interactive)
  (let ((controller (downcase
					 (read-from-minibuffer "Controller name: "))))
    (qz-open-clear-buffer (qz-name-view controller "home"))
	(qz-view-open "" "")
	(qz-insert 0 2 "<h3>Selamat datang.</h3>")
	(qz-view-close)))

(defun qz-create-login-view-index ()
  "Create login view index from controller."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (and (qz-table-p list-field)
				 (qz-login-actor-credential list-field))
			(let ((actor-credential (qz-login-actor-credential list-field))
				  (table (qz-name-table-from-id (car list-field))))
			  (qz-open-clear-buffer (qz-name-view controller "index"))
			  (qz-insert 0 1 "<!DOCTYPE html>")
			  (qz-insert 0 1 "<html lang=\"en\">")
			  (qz-insert 1 1 "<head>")
			  (qz-insert 2 1 "<meta charset=\"utf-8\">")
			  (qz-insert 2 1 "<meta http-equiv=\"X-UA-Compatible\" "
						 "content=\"IE=edge\">")
			  (qz-insert 2 1 "<meta name=\"viewport\" content="
						 "\"width=device-width, initial-scale=1\">")
			  (qz-insert 2 1 "<link rel=\"icon\" href=\"<?php echo "
						 "base_url();?>assets/favicon.ico\">")
			  (qz-insert 2 1 "<title>Halaman Login</title>")
			  (qz-insert 2 1 "<link href=\"<?php echo base_url();?>"
						 "assets/bootstrap/css/bootstrap.min.css\" rel="
						 "\"stylesheet\">")
			  (qz-insert 2 1 "<style>")
			  (qz-insert 3 1 "body, html { height: 100%; background-"
						 "repeat: no-repeat; background-image: linear"
						 "-gradient(rgb(104, 145, 162), "
						 "rgb(12, 97, 33)); }")
			  (qz-insert 3 1 ".card-container.card { max-width: 350px; "
						 "padding: 40px 40px; }")
			  (qz-insert 3 1 ".btn { font-weight: 700; height: 36px; "
						 "-moz-user-select: none; -webkit-user-select: "
						 "none; user-select: none; cursor: default; }")
			  (qz-insert 3 1 ".card { background-color: #F7F7F7; padding:"
						 " 20px 25px 30px; margin: 0 auto 25px; margin-"
						 "top: 50px; -moz-border-radius: 2px; -webkit-"
						 "border-radius: 2px; border-radius: 2px; -moz-"
						 "box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.3); "
						 "-webkit-box-shadow: 0px 2px 2px rgba(0, 0, 0, "
						 "0.3); box-shadow: 0px 2px 2px "
						 "rgba(0, 0, 0, 0.3); }")
			  (qz-insert 3 1 ".profile-img-card { width: 200px; height: "
						 "200px; margin: 0 auto 20px; display: block; "
						 "-moz-border-radius: 50%; -webkit-border-"
						 "radius: 50%; }")
			  (qz-insert 3 1 ".form-signin input[type=password], .form-"
						 "signin input[type=text], .form-signin button "
						 "{ width: 100%; display: block; margin-bottom: "
						 "10px; font-size: 16px; height: 44px; z-index: "
						 "1; position: relative; -moz-box-sizing: "
						 "border-box; -webkit-box-sizing: border-box; "
						 "box-sizing: border-box; }")
			  (qz-insert 3 1 ".form-signin .form-control:focus { border-"
						 "color: rgb(104, 145, 162); outline: 0; -webkit"
						 "-box-shadow: inset 0 1px 1px rgba(0,0,0,.075),"
						 "0 0 8px rgb(104, 145, 162); box-shadow: "
						 "inset 0 1px 1px rgba(0,0,0,.075),0 0 8px "
						 "rgb(104, 145, 162); }")
			  (qz-insert 3 1 ".btn.btn-signin { background-color: "
						 "rgb(104, 145, 162); padding: 0px; font-weight: "
						 "700; font-size: 14px; height: 38px; -moz-border"
						 "-radius: 3px; -webkit-border-radius: 3px; "
						 "border-radius: 3px; border: none; "
						 "-o-transition: all 0.218s; -moz-transition: "
						 "all 0.218s; -webkit-transition: "
						 "all 0.218s; transition: all 0.218s; }")
			  (qz-insert 3 1 ".btn.btn-signin:hover, .btn.btn-signin:"
						 "active, .btn.btn-signin:focus { "
						 "background-color: rgb(12, 97, 33); }")
			  (qz-insert 2 1 "</style>")
			  (qz-insert 1 1 "</head>")
			  (qz-insert 1 1 "<body>")
			  (qz-insert 2 1 "<div class=\"container\">")
			  (qz-insert 3 1 "<div class=\"card card-container\">")
			  (qz-insert 4 1 "<img class=\"profile-img-card\" "
						 "src=\"<?php echo base_url();?>assets/"
						 "image/logo.png\" />")
			  (qz-insert 4 1 "<form name=\"f_login\" method=\"post\" "
						 "action=\"\" class=\"form-signin\">")
			  (qz-insert 4 1 "<input type=\"text\" name="
						 (format "\"f_%s\" " (elt actor-credential 0))
						 "class=\"form-control\" placeholder="
						 (format "\"%s\" autofocus>"
								 (capitalize
								  (replace-regexp-in-string
								   (format "%s_" table)
								   "" (elt actor-credential 0)))))
			  (qz-insert 4 1 "<?php echo form_error"
						 (format "('f_%s');?>" (elt actor-credential 0)))
			  (qz-insert 4 1 "<input type=\"password\" name="
						 (format "\"f_%s\" " (elt actor-credential 1))
						 "class=\"form-control\" placeholder="
						 (format "\"%s\">"
								 (capitalize
								  (replace-regexp-in-string
								   (format "%s_" table)
								   "" (elt actor-credential 1)))))
			  (qz-insert 4 1 "<?php echo form_error"
						 (format "('f_%s');?>" (elt actor-credential 1)))
			  (qz-insert 4 1 "<button name=\"b_login\" "
						 "class=\"btn btn-lg btn-primary btn-block "
						 "btn-signin\" type=\"submit\" value="
						 "\"Login\">Login</button>")
			  (qz-insert 4 1 "<?php if ($pesan = @$this->session->"
						 "flashdata('pesan')) { ?>")
			  (qz-insert 5 1 "<span class=\"help-block error\"><?php "
						 "echo $pesan;?></span>")
			  (qz-insert 4 1 "<?php } ?>")
			  (qz-insert 3 1 "</form>")
			  (qz-insert 2 1 "</div>")
			  (qz-insert 1 1 "</div>")
			  (qz-insert 1 1 "</body>")
			  (qz-insert 0 1 "</html>"))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

(defun qz-create-login-view-password ()
  "Create login view password from controller."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region (region-beginning) (region-end)))
			(controller (downcase
						 (read-from-minibuffer "Controller name: "))))
		(if (and (qz-table-p list-field)
				 (qz-login-actor-credential list-field))
			(let ((actor-credential (qz-login-actor-credential list-field))
				  (table (qz-name-table-from-id (car list-field)))
				  repassword)
			  (qz-open-clear-buffer (qz-name-view controller "password"))
			  (qz-view-open (car list-field) "password")
			  (qz-insert 0 2 "<form class=\"form-horizontal\" "
						 "name=\"form_password\" "
						 "method=\"post\" action=\"\">")
			  (qz-insert 1 1 "<div class=\"form-group\">")
			  (qz-insert 2 1 "<label class=\"control-label col-sm-2\">"
						 (format "%s</label>"
								 (qz-label
								  (progn
									(string-match
									 "_"
									 (elt actor-credential 1))
									(substring
									 (elt actor-credential 1)
									 (match-end 0))))))
			  (qz-insert 2 1 "<div class=\"col-sm-10\">")
			  (qz-insert 3 1 "<input class=\"form-control\" "
						 "type=\"password\" ")
			  (qz-insert 4 1 (format "name=\"f_%s\" value=\"\"/>"
									 (elt actor-credential 1)))
			  (qz-insert 3 1 (format "<?php echo form_error('f_%s');?>"
									 (elt actor-credential 1)))
			  (qz-insert 2 1 "</div>")
			  (qz-insert 1 2 "</div>")
			  (setq repassword
					(replace-regexp-in-string
					 "\\(_\\).*\\'"
					 "_re_"
					 (elt actor-credential 1) nil nil 1))
			  (qz-insert 1 1 "<div class=\"form-group\">")
			  (qz-insert 2 1 "<label class=\"control-label col-sm-2\">"
						 (format
						  "%s</label>"
						  (qz-label
						   (progn
							 (string-match "_" repassword)
							 (substring repassword (match-end 0))))))
			  (qz-insert 2 1 "<div class=\"col-sm-10\">")
			  (qz-insert 3 1 "<input class=\"form-control\" "
						 "type=\"password\" ")
			  (qz-insert 4 1 (format "name=\"f_%s\" value=\"\"/>"
									 repassword))
			  (qz-insert 3 1 (format "<?php echo form_error('f_%s');?>"
									 repassword))
			  (qz-insert 2 1 "</div>")
			  (qz-insert 1 2 "</div>")
			  (qz-insert 1 1 "<div class=\"form-group\">")
			  (qz-insert 2 1 "<label class=\"control-label "
						 "col-sm-2\">&nbsp;</label>")
			  (qz-insert 2 1 "<div class=\"col-sm-10\">")
			  (qz-insert 3 1 "<input class=\"btn btn-primary\" type=\""
						 "submit\" name=\"b_password\" ")
			  (qz-insert 4 1 "value=\"Simpan\"/>")
			  (qz-insert 2 1 "</div>")
			  (qz-insert 1 2 "</div>")
			  (qz-insert 0 2 "</form>")
			  (qz-view-close))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

;; end qz-codeigniter.el
