# Envy

**Envy** is a very small library for building configurations from environments, which are [partial] mappings from string to string.

### Wait, do we really need another one of these?

Not really but I'm super picky, so here we are.

Envy is inspired by Ciris and is very similar in approach. The main difference is in the treatment of missing variables.

Consider the environment `{ "a" -> "...", "b" -> "..." }` and the following configurations:

```scala
val (a, b, c) = (env("a"), env("b"), env("c"))
val c1 = (a product (b or c))
val c2 = (a product b) or (a product c)
```

- In Ciris `c1` succeds because `c` is chosen as an alternative to [missing] `b`; however `c2` fails because it considers the absence of `"b"` in the *composition* `(a product b)` to be a fatal error. An incomplete configuration can reasonably be considered an error, so this is an entirely reasonable design choice.
- In Envy `c1` and `c2` are equivalent. I have found that I often have configurations with overlapping sets of variables and it works well when `product` and `<+>` distribute like `&&` and `||`. There is no right answer, I just like it better this way.
- Both libraries treat true errors (parsing/validation errors for instance) as fatal. Alternation is not suitable for error recovery.

### Laws

These laws hold, up to error-accumulation. Sequential composition is necessarily fast-fail; parallel composition accumulates errors. Here we use `×` for products (sequential or parallel, it doesn't matter) and `+` for alternation.

```scala
// a, b, c ∈ Env or Result
a × (b + c) = (a × b) + (a × c)      // product distributes over alternation

// a, b, c ∈ Env
a.run(e) × b.run(e) = (a × b).run(e)
a.run(e) + b.run(e) = (a + b).run(e)

// Note that alternation does not distribute over environments
a.run(e1 + e2) != a.run(e1) + a.run(e2)
```


### Medium-Speed Start

There is no doc to speak of. This will have to do for now.

- `Env[A]` is a computation which (given an environment) will attempt to compute a value of type `A`. It is little more than `(String => Option[String]) => (Trace, Option[A])`.
- `Env` accumulates a diagnostic `Trace` when it is run.
  - On success the trace is a record of how the configuration was constructed (occasionally useful).
  - On failure it is a record of what worked and what didn't, with the intent to guide the user to a solution (always useful).
- `Env` is
  - a `MonadError`, allowing for sequential composition with errors; and
  - a `Parallel`, allowing for parallel composition with `parMapN` and friends (prefer this when you can!); and
  - a `SemigroupK`, allowing for alternative ("or else") composition with `<+>`.
- `myEnv.load[F]` will run `myEnv` with the contents of `sys.env orElse sys.props` and yield the successful value or raise an error in `F` containing a textual representation of the failing trace. This is usually the way you want to do it.

### Inadequate Example

The example from above shows that `c1` and `c2` are equivalent up to tracing, which reveals distinct composition that reflects the code structure. We *could* normalize these expressions but I think it would make diagnosis harder. It's nice that the trace and source code line up.

```scala
scala> import envy._
     | import cats.implicits._
     | val (a, b, c) = (env("a"), env("b"), env("c"))
     | val c1 = (a product (b or c))
     | val c2 = (a product b) or (a product c)
...

scala> val vars = Map("a" -> "a", "c" -> "c").lift // String => Option[String]
...

scala> val (t, v) = c1.pureLoad(vars)
val t: envy.Trace = ...
val v: Option[(String, String)] = Some((a,c)) // Success!

scala> t.dump()
[x] all of the following
    [x] a
    [x] one of the following
        [ ] b
        [x] c

scala> val (t, v) = c2.pureLoad(vars)
val t: envy.Trace = ...
val v: Option[(String, String)] = Some((a,c))

scala> t.dump()
[x] one of the following
    [ ] all of the following
        [x] a
        [ ] b
    [x] all of the following
        [x] a
        [x] c
```

And an error case.

```scala
scala> val d = env("d").to[Int] // parse to Int
...

scala> val vars = Map("a" -> "a", "c" -> "c", "d" -> "abc").lift // "abc" isn't an valid Int!
...

scala> val (t, v) = (a, b, d, c).tupled.pureLoad(vars) // sequential composition!
val t: envy.Trace = ...
val v: Option[(String, String, Int, String)] = None // Failure!

scala> t.dump() // We can only trace up to the first uncomputable value
[ ] all of the following
    [x] a
    [ ] b
    [-] <sequential execution halted>

scala> val (t, v) = (a, b, d, c).parTupled.pureLoad(vars) // parallel composition!
val t: envy.Trace = ...
val v: Option[(String, String, Int, String)] = None // Still a failure!

scala> t.dump() // We see everything that went wrong
[!] all of the following
    [x] a
    [ ] b
    [!] d: invalid int: abc
    [x] c
```
