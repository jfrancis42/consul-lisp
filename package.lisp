;;;; package.lisp

(defpackage #:consul
  (:use #:cl)
  (:export
   :cdr-assoc ; maybe not export
   :parse-json-bytes ; maybe not export
   :consul
   :get-consul-kv-uri
   :kv-get
   :kv-put
   :kv-delete
   :tls
   :host
   :port
   :token
   :api-version
   :consul-kv-put-handler
   :consul-kv-get-handler
   :consul-kv-delete-handler
   ))
