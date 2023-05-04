package org.quasigroup.ibclient

import cats.effect.kernel.Resource
import cats.effect.{Clock, IO, SyncIO}
import org.quasigroup.ibclient.IBSocketClientCatsSuite.expect
import org.quasigroup.ibclient.impl.IBSocketClientCats
import org.scalacheck.Gen
import weaver.IOSuite
import weaver.scalacheck.*

object IBSocketClientCatsSuite extends IOSuite with Checkers:

  override type Res = IBClient[IO]

  override def sharedResource: Resource[IO, Res] = IBSocketClientCats.make[IO]()

  test("ibclient can request for current time") { ibclient =>
    for {
      now <- IO.realTimeInstant
      serverTime <- ibclient.reqCurrentTime
    } yield {
      expect(serverTime.time <= now.getEpochSecond)
    }
  }

  test("ibclient can request family codes") { ibclient =>
    for {
      familycodes <- ibclient.reqFamilyCodes
    } yield {
      expect(familycodes.familyCodes.length == 1)
    }
  }

//
//  test("ibclient can request to set server log level") { ibclient =>
//    for {
//      _ <- ibclient.setServerLogLevel(1)
//    } yield {
//      expect(true)
//    }
//  }

  test("ibclient can request positions") { ibclient =>
    for {
      positions <- ibclient.reqPositions.compile.toList
      _ <- ibclient.cancelPositions
    } yield {

      expect(positions.size >= 1)
    }
  }

  // test("ibclient can request for managed accounts") { ibclient =>
  //   for {
  //     accounts <- ibclient.reqManagedAccts
  //   } yield {
  //     expect(accounts.accountsList.nonEmpty)
  //   }
  // }

//  I don't have the FA account.
//  test("ibclient can request for fa ") { ibclient =>
//    for {
//      receivedFA <- ibclient.requestFA(1)
//    } yield {
//      expect(receivedFA.xml.nonEmpty)
//    }
//  }
