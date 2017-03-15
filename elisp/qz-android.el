;; Copyright (C) 2016  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Version: 0.0.2
;; Created: 08 December 2016
;; Keywords: android qzuma

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

;;; vars
(defvar qz-field-int
  ["^id" "_status$"])

(defvar qz-field-double
  ["latitude" "longitude"])

;;; functions
(defun qz-open-clear-buffer (name)
  "Open buffer with specific name."
  (switch-to-buffer-other-window name)
  (erase-buffer))

(defun qz-list-from-region (start end)
  "Get list from region."
  (split-string
   (buffer-substring-no-properties start end) "\n" t))

(defun qz-table-p (list-field)
  "Make sure the first is id_field and last is field_status."
  (if (and (string-match "^id_" (car list-field))
		   (string-match "_status$" (car (last list-field))))
	  t
	nil))

(defun qz-table-name (list-field)
  "Get table name from list-field."
  (replace-regexp-in-string
   "^id_" "" (car list-field)))

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
  (insert (qz-n-tab ntab))
  (mapc 'insert body)
  (insert (qz-n-eol neol)))

(defun qz-field-int-p (field)
  "Determine int or not."
  (let (p)
	(mapc
	 (lambda (f)
	   (when (string-match f field)
		 (setq p t)))
	 qz-field-int)
	p))

(defun qz-field-double-p (field)
  "Determine double or not."
  (let (p)
	(mapc
	 (lambda (f)
	   (when (string-match f field)
		 (setq p t)))
	 qz-field-double)
	p))

(defun qz-field-rm-table (field table)
  "Remove table name from field."
  (replace-regexp-in-string
   (concat "[_]?" table "[_]?")
   "" field))

(defun qz-field-to-tag (field &optional table)
  "Format table field to be tag."
  (unless table
	(setq table "\""))
  (setq field
		(qz-field-rm-table
		 field table))
  (cond
   ((string-match "^id_" field)
	(concat (substring field 3) "_id"))
   (t field)))

(defun qz-fields-to-tags (list-field)
  "Get list tag from list field."
  (let ((table (qz-table-name list-field))
		(fields (cdr list-field))
		new-fields)
	(setq new-fields
		  (mapcar
		   (lambda (f)
			 (qz-field-to-tag f table))
		   fields))
	(push "id" new-fields)))

(defun qz-row-static-tag (tag field)
  "Row static tag."
  (concat "public static final String TAG_"
		  (upcase tag) " = \"" field "\";"))

(defun qz-rows-static-tags (list-field)
  "List static tags from fields."
  (let ((fields list-field)
		(tags (qz-fields-to-tags list-field))
		static-tags)
	(cl-mapcar 'qz-row-static-tag tags fields)))

(defun qz-class-field (field &optional table)
  "Class field from field."
  (unless table
	(setq table "\""))
  (concat
   "m"
   (mapconcat 'capitalize
			  (split-string
			   (qz-field-rm-table field table)
			   "_" t) "")))

(defun qz-class-fields (list-field)
  "Get list class field from list field."
  (let ((table (qz-table-name list-field))
		(fields (cdr list-field))
		new-fields)
	(setq new-fields
		  (mapcar
		   (lambda (f)
			 (qz-class-field f table))
		   fields))
	(push "mId" new-fields)))

(defun qz-field-type (field)
  "Get field type."
  (cond ((qz-field-int-p field)
		 "int")
		((qz-field-double-p field)
		 "Double")
		(t "String")))

(defun qz-input-field (class-field)
  "Format class-field to input-field."
  (concat (downcase (substring class-field 1 2))
		  (substring class-field 2)))

(defun qz-row-class-field (class-field field)
  "Row class field."
  (format "private %s %s;"
		  (qz-field-type field)
		  class-field))

(defun qz-rows-class-fields (list-field)
  "List class field from list field."
  (let ((fields list-field)
		(class-fields (qz-class-fields list-field)))
	(cl-mapcar 'qz-row-class-field class-fields fields)))

(defun qz-class-field-to-class-method (class-field get-or-set)
  "Change class-field to class-method."
  (concat get-or-set (substring class-field 1)))

(defun qz-getter (field &optional table)
  "Create getter from field."
  (unless table
	(setq table "\""))
  (let ((type (qz-field-type field))
		(method (qz-class-field-to-class-method
				 (qz-class-field field table) "get"))
		(class-field (qz-class-field field table)))
	(list (format "public %s %s() {" type method)
		  (format "%sreturn %s;" (qz-n-tab 1) class-field)
		  "}")))

(defun qz-setter (field &optional table)
  "Create setter from field."
  (unless table
	(setq table "\""))
  (let ((type (qz-field-type field))
		(method (qz-class-field-to-class-method
				 (qz-class-field field table) "set"))
		(class-field (qz-class-field field table)))
	(list (format "public void %s(%s %s) {"
				  method type
				  (qz-input-field class-field))
		  (format "%sthis.%s = %s;"
				  (qz-n-tab 1)
				  class-field
				  (qz-input-field class-field))
		  "}")))

(defun qz-getters-setters (list-field &optional ntab)
  "Create all getters and setters."
  (unless ntab
	(setq ntab 0))
  (let ((table (qz-table-name list-field)))
  	(mapc (lambda (f)
			(mapc (lambda (l) (qz-insert ntab 1 l))
				  (qz-getter f table))
			(qz-insert 0 1 "")
			(mapc (lambda (l) (qz-insert ntab 1 l))
				  (qz-setter f table))
			(qz-insert 0 1 ""))
  		  list-field)))

(defun qz-model-file (name)
  "Rename buffer file."
  (concat (capitalize name) ".java"))

(defun qz-model-open (package class)
  "Opening model class."
  (qz-insert 0 2 (format "package %s;" package))
  (qz-insert 0 2 "import java.io.Serializable;")
  (qz-insert 0 1 "@SuppressWarnings(\"all\")")
  (qz-insert 0 2 "public class"
			 (format " %s " (capitalize class))
			 "implements Serializable {")
  (qz-insert 1 2 "private static final "
			 "long serialVersionUID = 0L;"))

(defun qz-model-to-string (list-field)
  "Overide object to string."
  (let ((tags (qz-fields-to-tags list-field))
		(class-fields (qz-class-fields list-field)))
	(qz-insert 1 1 "@Override")
	(qz-insert 1 1 "public String toString() {")
	(qz-insert 2 1 "return \"{\"")
	(cl-mapc (lambda (ct cf)
			   (qz-insert 3 1 "+ '\"' + "
						  (format "TAG_%s " (upcase ct))
						  "+ '\"' + ':' + '\"' + "
						  (format "%s + " cf)
						  (if (equal ct (car (last tags)))
							  "'\"' + '}';"
							"'\"' + ','")))
			 tags class-fields)
	(qz-insert 1 1 "}")))

(defun qz-model-adapter-file (name)
  "Rename buffer file"
  (concat (capitalize name) "Adapter.java"))

(defun qz-model-adapter-open (package class)
  "Opening adapter for model class."
  (qz-insert 0 2 (format "package %s;" package))
  (qz-insert 0 1 "import android.content.Context;")
  (qz-insert 0 1 "import android.view.LayoutInflater;")
  (qz-insert 0 1 "import android.view.View;")
  (qz-insert 0 1 "import android.view.ViewGroup;")
  (qz-insert 0 1 "import android.widget.BaseAdapter;")
  (qz-insert 0 2 "import android.widget.TextView;")
  (qz-insert 0 2 "import java.util.List;")
  (qz-insert 0 1 "@SuppressWarnings(\"all\")")
  (qz-insert 0 2 "public class"
			 (format " %sAdapter " (capitalize class))
			 "extends BaseAdapter {")
  (qz-insert 1 1 "private Context mContext;")
  (qz-insert 1 2 "private List"
			 (format "<%s> mItems;" (capitalize class)))
  (qz-insert 1 1 (format "public %s" (capitalize class))
			 "Adapter(Context c, List"
			 (format "<%s> l%s) {"
					 (capitalize class)
					 (substring class 0 1)))
  (qz-insert 2 1 "this.mContext = c;")
  (qz-insert 2 1 "this.mItems = "
			 (format "l%s;" (substring class 0 1)))
  (qz-insert 1 2 "}")
  (qz-insert 1 1 "@Override")
  (qz-insert 1 1 "public int getCount() {")
  (qz-insert 2 1 "return mItems.size();")
  (qz-insert 1 2 "}")
  (qz-insert 1 1 "@Override")
  (qz-insert 1 1 "public Object getItem(int position) {")
  (qz-insert 2 1 "return mItems.get(position);")
  (qz-insert 1 2 "}")
  (qz-insert 1 1 "@Override")
  (qz-insert 1 1 "public long getItemId(int position) {")
  (qz-insert 2 1 "return mItems.get(position).getId();")
  (qz-insert 1 2 "}")
  (qz-insert 1 1 "@Override")
  (qz-insert 1 2 "public View getView(int position, View "
			 "view, ViewGroup parent) {")
  (qz-insert 2 1 "if (view == null) {")
  (qz-insert 3 1 "view = LayoutInflater.from(mContext)")
  (qz-insert 4 1 ".inflate(android.R.layout."
			 "simple_list_item_2, parent, false);")
  (qz-insert 2 2 "}")
  (qz-insert 2 1 (format "%s %s = mItems.get(position);"
						 (capitalize class)
						 (substring class 0 1)))
  (qz-insert 2 1 "TextView tv1 = (TextView) view."
			 "findViewById(android.R.id.text1);")
  (qz-insert 2 1 "TextView tv2 = (TextView) view."
			 "findViewById(android.R.id.text2);"))

(defun qz-adapter-two-text-field (class list-field)
  "Get formatted two field model."
  (let ((table (qz-table-name list-field))
		(field-1 (elt list-field 1))
		(field-2 (elt list-field 2)))
	(qz-insert 0 1 "")
	(qz-insert 2 1
			   (format
				"tv1.setText(%s.%s());"
				(substring class 0 1)
				(qz-class-field-to-class-method
				 (qz-class-field field-1 table) "get")))
	(qz-insert 2 1
			   (format
				"tv2.setText(%s.%s());"
				(substring class 0 1)
				(qz-class-field-to-class-method
				 (qz-class-field field-2 table) "get")))))

(defun qz-model-helper-file (name)
  "Rename buffer file"
  (concat (capitalize name) "Helper.java"))

(defun qz-model-helper-open (package class)
  "Opening helper for model class."
  (qz-insert 0 2 (format "package %s;" package))
  (qz-insert 0 1 "import android.content.ContentValues;")
  (qz-insert 0 2 "import android.os.AsyncTask;")
  (qz-insert 0 1 "import org.json.JSONObject;")
  (qz-insert 0 1 "import org.json.JSONArray;")
  (qz-insert 0 2 "import org.json.JSONException;")
  (qz-insert 0 1 "import java.util.List;")
  (qz-insert 0 2 "import java.util.ArrayList;")
  (qz-insert 0 1 "import com.gmail.epanji.koneksiurl.KoneksiUrl;")
  (qz-insert 0 2 (format "import %s.%s;"
						 package (capitalize class)))
  (qz-insert 0 1 "@SuppressWarnings(\"all\")")
  (qz-insert 0 2 "public class"
			 (format " %sHelper " (capitalize class))
			 "extends AsyncTask<Void, Void, "
			 (format "List<%s>> {" (capitalize class)))
  (qz-insert 1 1 "private static final String "
			 "TAG_RESULT = \"result\";")
  (qz-insert 1 1 "private static final String "
			 "TAG_RESULT_TRUE = \"true\";")
  (qz-insert 1 2 "private static final String "
			 "TAG_DATA = \"data\";")
  (qz-insert 1 2 "private ContentValues mParams;")
  (qz-insert 1 1 (format "public %s" (capitalize class))
			 "Helper(ContentValues cv) {")
  (qz-insert 2 1 "this.mParams = cv;")
  (qz-insert 1 2 "}")
  (qz-insert 1 1 "@Override")
  (qz-insert 1 1 "protected "
			 (format "List<%s>" (capitalize class))
			 " doInBackground(Void... params) {")
  (qz-insert 2 1 "KoneksiUrl kUrl = new KoneksiUrl();")
  (qz-insert 2 1 "String jsonStr = kUrl.execute("
			 "UrlHelper.URL_SERVICE_"
			 (format "%s, mParams);" (upcase class)))
  (qz-insert 2 1 (format "List<%s> " (capitalize class))
			 (format "l%s = " (substring class 0 1))
			 "new ArrayList<>();")
  (qz-insert 2 1 "try {")
  (qz-insert 3 1 "JSONObject jo = new JSONObject(jsonStr);")
  (qz-insert 3 1 "String result = jo.getString(TAG_RESULT);")
  (qz-insert 3 1 "if (result.equalsIgnoreCase(TAG_RESULT_TRUE)) {")
  (qz-insert 4 1 "JSONArray ja = jo.getJSONArray(TAG_DATA);")
  (qz-insert 4 1 "for(int i=0; i<ja.length(); i++) {")
  (qz-insert 5 1 "JSONObject o = ja.getJSONObject(i);"))

(defun qz-value-of-type (field)
  "Type on value of"
  (cond ((qz-field-int-p field)
		 "Integer.valueOf(")
		((qz-field-double-p field)
		 "Double.valueOf(")
		(t "")))

(defun qz-model-helper-setter (class list-field)
  "Looping fields for json."
  (let ((class-fields (qz-class-fields list-field))
		(tags (qz-fields-to-tags list-field)))
	(qz-insert 0 1 "")
	(qz-insert 5 2 (format "%s %s = new %s();"
						   (capitalize class)
						   (substring class 0 1)
						   (capitalize class)))
	(cl-mapc
	 (lambda (cf ct lf)
	   (let ((valof (qz-value-of-type lf)))
		(qz-insert 5 1 "if (o.has"
				   (format "(%s.TAG_%s))"
						   (capitalize class)
						   (upcase ct)))
		(qz-insert 6 1
				   (format "%s.set%s"
						   (substring class 0 1)
						   (substring cf 1))
				   "("
				   (format "%s" valof)
				   "o.getString"
				   (format "(%s.TAG_%s))"
						   (capitalize class)
						   (upcase ct))
				   (if (equal valof "")
					   ";"
					 ");")
				   )))
	 class-fields tags list-field)
	(qz-insert 0 1 "")
	(qz-insert 5 1
			   (format "l%s.add(%s);"
					   (substring class 0 1)
					   (substring class 0 1)))))

;;; functions
(defun qz-create-android-model
	(package class list-field with-status-p)
  "Create android model."
  (unless with-status-p
	(setq list-field (butlast list-field)))
  (qz-open-clear-buffer (qz-model-file class))
  (qz-model-open package class)
  (mapc (lambda (f) (qz-insert 1 1 f))
		(qz-rows-static-tags list-field))
  (qz-insert 0 1 "")
  (mapc (lambda (f) (qz-insert 1 1 f))
		(qz-rows-class-fields list-field))
  (qz-insert 0 1 "")
  (qz-getters-setters list-field 1)
  (qz-model-to-string list-field)
  (qz-insert 0 0 "}")
  (when (fboundp 'java-mode) (java-mode)))

(defun qz-create-android-model-adapter
	(package class list-field)
  "Create android adapter for specific model"
  (qz-open-clear-buffer (qz-model-adapter-file class))
  (qz-model-adapter-open package class)
  (qz-adapter-two-text-field class list-field)
  (qz-insert 0 1 "")
  (qz-insert 2 1 "return view;")
  (qz-insert 1 1 "}")
  (qz-insert 0 0 "}")
  (when (fboundp 'java-mode) (java-mode)))

(defun qz-create-android-model-helper
	(package class list-field with-status-p)
  "Create android helper for specific model"
  (unless with-status-p
	(setq list-field (butlast list-field)))
  (qz-open-clear-buffer (qz-model-helper-file class))
  (qz-model-helper-open package class)
  (qz-model-helper-setter class list-field)
  (qz-insert 4 1 "}")
  (qz-insert 3 1 "}")
  (qz-insert 2 2 "} catch (JSONException ignored){}")
  (qz-insert 2 1 (format "return l%s;"
						 (substring class 0 1)))
  (qz-insert 1 1 "}")
  (qz-insert 0 0 "}")
  (when (fboundp 'java-mode) (java-mode)))

;;; commands
(defun qz-create-android-model-adapter-helper ()
  "Create android model adapter and helper."
  (interactive)
  (if (use-region-p)
	  (let ((list-field
			 (qz-list-from-region
			  (region-beginning)
			  (region-end)))
			(package (downcase
					  (read-from-minibuffer
					   "Package name: "
					   "id.android.name")))
			(class (downcase
					(read-from-minibuffer
					 "Class name: ")))
			(with-status-p (y-or-n-p "With status? ")))
		(if (qz-table-p list-field)
			(progn
			  (qz-create-android-model
			   package class list-field with-status-p)
			  (qz-create-android-model-adapter
			   package class list-field)
			  (qz-create-android-model-helper
			   package class list-field with-status-p))
		  (print "Selected region not well formatted")))
	(print "No region selected")))

;;; end qz-android.el
