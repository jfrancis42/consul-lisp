;;;; package.lisp

(defpackage #:consul
  (:use #:cl)
  (:export
   :cdr-assoc ; maybe not export
   :parse-json-bytes ; maybe not export
   :join ; maybe not export
   :as-string ; maybe not export
   :file-string ; maybe not export
   :consul
   :get-consul-kv
   :kv-get
   :kv-put
   :kv-delete
   :tls
   :host
   :port
   :token
   :api-version
   ))
