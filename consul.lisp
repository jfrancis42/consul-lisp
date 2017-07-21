;;;; consul.lisp

(in-package #:consul)

;;; "consul" goes here. Hacks and glory await!

(defmacro cdr-assoc (name alist)
  "Replaces '(cdr (assoc name alist))' because it's used a bajillion
times when doing API stuff."
  `(cdr (assoc ,name ,alist :test #'equal)))

(defun parse-json-bytes (text)
  "Parse a list of bytes as a JSON string and return the decoded JSON."
  (json:decode-json-from-string (babel:octets-to-string text)))

(defun join (stuff separator)
  "Join a list of strings with a separator (like ruby string.join())."
  (with-output-to-string (out)
    (loop (princ (pop stuff) out)
       (unless stuff (return))
       (princ separator out))))

(defun as-string (n)
  "Return n as a string (empty string if n is nil)."
  (if n (format nil "~A" n) ""))

(defun file-string (path)
  "Return contents of a file as a single string."
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defclass consul ()
;;; "A class to hold consul credentials and information."
  ((tls :accessor tls
	:initarg :tls
	:initform nil)
   (host :accessor host
	 :initarg :host
	 :initform nil)
   (port :accessor port
	 :initarg :port
	 :initform nil)
   (token :accessor token
	  :initarg :token
	  :initform nil)
   (api-version :accessor api-version
		:initarg :api-version
		:initform nil)))

(defmethod get-consul-kv-uri ((c consul))
  "Return the URI of the consul server in the form http://consul.foo.com:8500/v1/kv/"
  (concatenate 'string
	       (if (tls c) "https://" "http://")
	       (if (host c) (host c) "localhost")
	       ":"
	       (if (port c) (format nil "~A" (port c)) "8500")
	       (if (api-version c)
		   (concatenate 'string "/v" (format nil "~A" (api-version c)) "/")
		   "/v1/")
	       "kv/"))

(defun kv-put (creds var what)
  "Set the variable 'var' to the value 'what' on the consul server 'creds'."
  (json:decode-json-from-string
   (babel:octets-to-string
    (nth-value 0
	       (drakma:http-request (concatenate 'string (get-consul-kv-uri creds) var)
				    :method :put
				    :accept "application/json"
				    :content-type "application/json"
				    :content what)))))

(defun kv-get (creds var &key (recurse nil) (keys nil))
  "Retrieve the value of the variable 'var' from the consul server
'creds'. Recurse if :recurse is t, and return only the keys if :keys
is t. Do not specify both :recurse and :keys (when you
use :keys, :recurse is automatically assumed)."
  (multiple-value-bind (body status)
      (drakma:http-request (concatenate 'string (get-consul-kv-uri creds) var
					(if recurse "?recurse=true" "")
					(if keys "?keys=true" ""))
			   :method :get
			   :accept "application/json"
			   :content-type "application/json")
    (if (equal 404 status)
	(values nil nil)
	(let ((payload (json:decode-json-from-string
			(babel:octets-to-string
			 (nth-value 0 body)))))
	(cond
	  (keys
	   payload)
	  (recurse
	   (mapcar
	    (lambda (a)
	      (cons
	       (consul:cdr-assoc :*key a)
	       (cl-base64:base64-string-to-string (consul:cdr-assoc :*value a)))) payload))
	  (t
	   (values 
	    (cl-base64:base64-string-to-string
	     (cdr-assoc :*value
			(first payload)))
	    (cdr-assoc :*modify-index
		       (first payload)))))))))

(defun kv-delete (creds var)
  "Delete the variable 'var' from the consul server 'creds'."
  (json:decode-json-from-string
   (babel:octets-to-string
    (nth-value 0
	       (drakma:http-request (concatenate 'string (get-consul-kv-uri creds) var)
				    :method :delete
				    :accept "application/json"
				    :content-type "application/json")))))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
