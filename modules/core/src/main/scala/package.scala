// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

import cats.data._
import cats.implicits._

package object envy {

  // def key

  def env(key: Key): Env[String] =
    Env { env =>
      val op = env(key.name)
      (Trace.Read(key, op), op)
    }

  def env(name: String): Env[String] =
    env(Key(name, None))

  def error[A](message: String): Env[A] =
    Env.MonadErrorEnv.raiseError(NonEmptyChain(message))

  def pure[A](value: A): Env[A] =
    value.pure[Env]


}
