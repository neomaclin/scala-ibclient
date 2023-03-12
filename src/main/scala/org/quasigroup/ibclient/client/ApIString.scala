package org.quasigroup.ibclient.client

trait ApIString[A]:
  extension(a: A) def asApiString: String
