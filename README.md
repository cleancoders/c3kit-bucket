# Bucket

![Bucket](https://github.com/cleancoders/c3kit/blob/master/img/bucket_200.png?raw=true)

A Clojure library component of [c3kit - Clean Coders Clojure Kit](https://github.com/cleancoders/c3kit).

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
