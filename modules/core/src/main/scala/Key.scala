// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package envy

case class Key(
  name: String,
  description: Option[String] = None,
  secret: Boolean = false
)
