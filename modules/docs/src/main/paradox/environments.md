# Environments

Environments are the inputs to Envy. They are partial mapping from `String` to `String`, which
accommodates a lot of common sources for application configuration, including properties files,
environment variables, system properties, string maps, and so on. Envy provides accessors for some
commonly used environments.

| Accessor | Description |
|---|---|
| `Env.sysEnv`    | System environment.     |
| `Env.javaProps` | Java system properties. |
| `Env.javaPropsF` | Snapshot of Java system properties in `F: Sync` (see note below). |
| `Env.fromMap`   | Turn a `Map[String, String]` into an `Env`. |
| `Env.fromFunction`   | Turn a `String => Option[String]` into an `Env`. |
| `Env.fromPartialFunction`   | Turn a `PartialFunction[String, String]` into an `Env`. |

Strictly speaking `javaProps` is a side-effect because the contents _can_ change at runtime. If
you're doing this you should use `javaPropsF` to take a snapshot, otherwise your configuration will
be nondeterministic.

## Composing Environments

`Env` composes via `orElse` alternation, written `<+>`.

```scala
// Read from system environment, falling back to Java system properties
val myEnv = Env.sysEnv <+> Env.javaProps
```

For less common situations you can write out the precise logic you need.

```scala
// Require value from system environment, but allow overriding via Java
// system properties
val myEnv: Env =
  Env.fromFunction { s =>
    Env.sysEnv(s).map { n =>
      Env.javaProps(s).getOrElse(n)
    }
  }
```