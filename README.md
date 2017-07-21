# Consul

This is a library to support HashiCorp's Consul.  At this stage, only
getting/putting/deleting KV values is supported, but full support for
service discovery is in the works.

Configuration and use of the library is extremely simple.  You'll need
to create a consul object (which holds the information about a given
consul server), then make kv-get, kv-put, and kv-delete calls to get,
set, and delete values.  When instantiating the consul object, it
assumes sane defaults if you don't supply any parameters.
Specifically, if instantiated with not parameters, it assumes no TLS,
a host of "localhost", a port of 8500, and a protocol version "1".
There is a field for an authentication token, but this is unused in
this version of code (will be supported in future versions).

```
CL-USER> (defvar *consul* (make-instance 'consul:consul :host "consul.example.com" :tls nil))
*CONSUL*
CL-USER>
```

Once the consul object has been created, it can be used to get, set,
and delete consul KV values:

```
CL-USER> (consul:kv-get *consul* "electric_boogaloo")
NIL
NIL
CL-USER> (consul:kv-put *consul* "electric_boogaloo" "Test data.")
T
CL-USER> (consul:kv-get *consul* "electric_boogaloo")
"Test data."
6422
CL-USER> (consul:kv-delete *consul* "electric_boogaloo")
T
CL-USER> (consul:kv-get *consul* "electric_boogaloo")
NIL
NIL
CL-USER> 
```

You can also ask for a recursive result. Recursive results are
returned as a list of conses of key:value pairs (note that this can be
a very expensive operation in time, client memory, and server
resources if the values are large - use with extreme caution):

```
CL-USER> (consul:kv-put *consul* "foo" "test value 1")
T
CL-USER> (consul:kv-put *consul* "bar" "test value 2")
T
CL-USER> (consul:kv-put *consul* "baz" "test value 3")
T
CL-USER> (consul:kv-get *consul* "/" :recurse t)
(("bar" . "test value 2") ("baz" . "test value 3") ("foo" . "test value 1"))
CL-USER> 
```

Finally, you can ask for just a list of keys:

```
(consul:kv-get *consul* "/" :keys t)
("bar" "baz" "foo")
CL-USER> 
```

That's it at this point. Future features will include support for
services, health checks, and watchers for KV variables with callbacks
of arbitrary functions.

## ToDo

* Support for KV value watchers (with callbacks)
* Support cas for puts
* Support cas for deletes
* Support flags for gets
* Support flags for puts
* Support acquire for puts
* Support release for puts
* Support recurse for delete
* Support for Services
* Support for Health Checks
* Support for tokens
