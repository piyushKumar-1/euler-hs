# Database Layering in the Euler Stack

There are three layers necessary for the Order API and other current and future
Juspay APIs:

  1. Raw SQL layer
  2. DB mesh mode
  3. Redis caching

These issues are separate and deserve separate treatment, but can be layered
on top of each other.

The proposal is detailed below.

# 1. DB/SQL Layer (Beam)

The DB/SQL layer in `euler-hs` uses `Beam` and is to stay as-is.
Beam will be used for schemas, and the recommendation will remain to convert 
storage types into domain types so as to:

  1. provide a separation of concerns / proper layering
  2. not leak storage (and Beam) types through application code/business logic

# 2. Caching Layer

Caching layer ala `presto-backend` as follows, to be added to the `Flow` framework
in `euler-hs`:

  1. use Beam storage types
  2. use [Artyom's Sequelize port](https://bitbucket.org/juspay/haskell-sequelize/src/master/) for queries
  3. *explicit* cache keys
  4. automated Redis caching

Interface is the same as presto-backend, modulo minor changes to the Sequelize
query language and modulo explicit cache keys:

```haskell
create :: (ToJSON a, FromJSON a) => Model a -> Text -> a -> Flow a

updateOne :: (ToJSON a, FromJSON a) => Model a -> Text -> a -> WhereClause -> Flow (Maybe a)
updateOne dbTable cacheKey value whereClause = ...

findOne :: (ToJSON a, FromJSON a) => Model a -> Text -> WhereClause -> Flow (Maybe a)
findOne dbTable cacheKey whereClause = ...

findAll :: (ToJSON a, FromJSON a) => Model a -> Text -> WhereClause -> Flow [a]
findAll dbTable cacheKey whereClause = ...
```

This will be the recommended way of talking to databases for the majority of
queries across Euler, and across Juspay projects built on top of `euler-hs` and
the `Flow` framework. The Beam layer will remain available for more fine-grained
control, database joins, etc.

# 3. DB Mesh
A new layer with the same interface as the caching layer, but using business
logic and runtime configuration data to decide for which tables and under
what conditions to route DB traffic to a redis stream, and under which conditions
missing cache keys result in an exception.

Although explicit cache keys are provided, there is a flag `ENSURE_PS_COMPAT`
that serializes the `WHERE` clause for use as the DB-mesh/cache key to ensure
compatibility with `euler-ps`. Once `euler-ps` is fully decomissioned we can
set this flag to `False` and only rely on explicitly provided cache keys.

## Why Explicit Cache Keys?
The philosophical answer is that the cache key should be part of the caching
interface and subject to the contractual obligations between upstream producers
and downstream consumers.

The practical answer is that explicit cache keys are more robust, easier to
debug, easier to keep consistent across versions, independent of the query itself,
independent of the query implementation and independent of the query 
serialization routines.
