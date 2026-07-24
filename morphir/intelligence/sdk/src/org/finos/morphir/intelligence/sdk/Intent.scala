
package org.finos.morphir.intelligence.sdk

import kyo.Schema

enum Intent derives Schema:
  case New(title: String, description: String)
