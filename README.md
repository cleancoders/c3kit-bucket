# Bucket

![Bucket](https://github.com/cleancoders/c3kit/blob/master/img/bucket_200.png?raw=true)

A library component of [c3kit - Clean Coders Clojure Kit](https://github.com/cleancoders/c3kit).

_"Most men give advice by the bucket, but take it by the grain."_ - William R. Alger

Bucket offers an identical API for dealing with data on both the server side (datomic), and the client side (in memory), in addition to other goodies.

 * __bg.clj__ : background task management
 * __db.clj__ : simple api for datomic interaction
 * __migrate.clj__ : manage and execute migration scripts
 * __spec_helper.clj__ : to easily test client code
 * __hashid.cljc__ : platform independent hashid
 * __db.cljs__ : simple api for in-memory data storage and retrieval
 * __spec_helper.cljs__ : to easily test client code

# Development

    # Run the JVM tests
    clj -M:test:spec
    clj -M:test:spec -a         # auto runner

    # Compile and Run JS tests
    clj -M:test:cljs once
    clj -M:test:cljs            # auto runner

# Deployment

In order to deploy to c3kit you must be a member of the Clojars group `com.cleancoders.c3kit`.

1. Go to https://clojars.org/tokens and configure a token with the appropriate scope
2. Set the following environment variables

```
CLOJARS_USERNAME=<your username>
CLOJARS_PASSWORD=<your deploy key>
```

3. Update VERSION file
4. `clj -T:build deploy`

