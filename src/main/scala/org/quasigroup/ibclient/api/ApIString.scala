package org.quasigroup.ibclient.api

trait ApIString[A]:
  extension(a: A) def asApiString: String
