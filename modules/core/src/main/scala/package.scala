// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

import cats.implicits._

package object envy {

  def env(key: Key): Env[String] =
    Env { env =>
      env(key.name) match {
          case None    => (Trace.Missing(key), Some(key), None)
          case Some(s) => (Trace.Success(key), Some(key), Some(s))
        }
    }

  def env(name: String): Env[String] =
    env(Key(name, None))

  def default[A](value: A): Env[A] =
    value.pure[Env]

  type ConfigValue[A] = Env[A]
  type ConfigDecoder[A, B] = EMapper[A, B]
  val  ConfigDecoder = EMapper
}
