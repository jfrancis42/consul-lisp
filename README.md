# Consul

This is a library to support HashiCorp's Consul. At this stage, only
getting/putting KV values are supported, but full support for service
discovery is in the works.

At this point, configuration and use of the library are extremely
simple. You'll need to create a consul object (which holds the
information about a given consul server), then make kv-get and kv-put
calls to get and set values. When instantiating the consul object, it
assumes sane defaults if you don't supply any
parameters. Specifically, if instantiated with not parameters, it
assumes no TLS, a host of "localhost", a port of 8500, and a protocol
version "1". There is a field for an authentication token, but this is
unused in this version of code (will be supported in future versions).

```
CL-USER> (defvar *consul* (make-instance 'consul:consul :host "consul.example.com" :tls nil))
*CONSUL*
CL-USER> 
```

Once the consul object has been created, it can be used to get and set
consul KV values:

```
CL-USER> (consul:kv-get *consul* "electric_boogaloo")
NIL
CL-USER> (consul:kv-put *consul* "electric_boogaloo" "Test data.")
T
CL-USER> (consul:kv-get *consul* "electric_boogaloo")
"Test data."
CL-USER> 
```

That's it at this point. Future features will include support for
services, health checks, and watchers for KV variables with callbacks
of arbitrary functions.

## ToDo

* Support for Services
* Support for Health Checks
* Support for KV value watchers