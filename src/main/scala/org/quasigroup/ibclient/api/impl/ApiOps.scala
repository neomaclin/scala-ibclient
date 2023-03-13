package org.quasigroup.ibclient.api.impl
import cats.free.Free
sealed trait ApiOps[A]
type ApiFreeOps[A] = Free[ApiOps,A]

object ApiOps{

}
