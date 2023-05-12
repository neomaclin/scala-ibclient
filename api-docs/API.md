API: Architecture 

Interface

Implementation

Establishing an API connection
 
Client will be obtained as a resource via

```Scala
val client: Resource[F, IBClient[F]] = IBSocketClientCats.make[F]()
```

with defaults as followed, override to you envirment as needed.
```Scala
    host: Host = host"127.0.0.1",
    port: Port = port"7496",
    optionalCapabilities: Option[String] = None,
    clientId: Int = 10
```      


Request Only

Request and Response

Request for Data Stream