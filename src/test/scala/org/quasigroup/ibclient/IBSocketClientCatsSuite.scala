package org.quasigroup.ibclient

import cats.effect.kernel.Resource
import cats.effect.{Clock, IO, SyncIO}
import org.quasigroup.ibclient.IBSocketClientCatsSuite.expect
import org.quasigroup.ibclient.client.IBClient
import org.quasigroup.ibclient.client.impl.IBSocketClientCats
import org.scalacheck.Gen
import weaver.IOSuite
import weaver.scalacheck.*

object IBSocketClientCatsSuite extends IOSuite with Checkers {

  override def maxParallelism: Int = 1

  override type Res = IBClient[IO]

  override def sharedResource: Resource[IO, Res] = IBSocketClientCats.make[IO]()

  test("ibclient can request for current time") { ibclient =>
    for {
      now <- IO.realTimeInstant
      serverTime <- ibclient.reqCurrentTime()
    } yield {
      expect(serverTime.time <= now.getEpochSecond)
    }
  }

  test("ibclient can request family codes") { ibclient =>
    for {
      familycodes <- ibclient.reqFamilyCodes()
    } yield {
      expect(familycodes.familyCodes.isEmpty)
    }
  }

}
