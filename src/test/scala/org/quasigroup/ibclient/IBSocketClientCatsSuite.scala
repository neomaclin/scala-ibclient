package org.quasigroup.ibclient

import org.quasigroup.ibclient.types.*
import org.quasigroup.ibclient.types.TypesCodec.given

import org.quasigroup.ibclient.encoder.Encoder
import org.quasigroup.ibclient.encoder.Encoder.{*, given}
import org.quasigroup.ibclient.decoder.Decoder
import org.quasigroup.ibclient.decoder.Decoder.{*, given}
import org.quasigroup.ibclient.request.RequestMsg
import org.quasigroup.ibclient.request.RequestMsg.*
import org.quasigroup.ibclient.request.MsgEncoders.{*, given}
import org.quasigroup.ibclient.response.ResponseMsg
import org.quasigroup.ibclient.response.ResponseMsg.*
import org.quasigroup.ibclient.response.MsgDecoders.given

import cats.syntax.all.*
import cats.effect.kernel.Resource
import cats.effect.{Clock, IO, SyncIO}
import cats.effect.std.Random
import org.quasigroup.ibclient.IBSocketClientCatsSuite.expect
import org.quasigroup.ibclient.impl.IBSocketClientCats
import org.scalacheck.Gen
import weaver.IOSuite
import weaver.scalacheck.*

object IBSocketClientCatsSuite extends IOSuite with Checkers with ContractSpec with OrderSpec:

  override type Res = (IBClient[IO], Random[IO])

  override def sharedResource: Resource[IO, Res] =
    IBSocketClientCats.make[IO]().both(Resource.eval(Random.scalaUtilRandom[IO]))

  test("obtain histogram") { case (ibclient, random) =>
    for {
      tickerId <- random.nextInt
      _ <- ibclient.requestOnly[ReqHistogramData](
        ReqHistogramData(tickerId = tickerId, contract = USStock, useRTH = false, timePeriod = "3 days")
      )
      _ <- ibclient.requestOnly[CancelHistogramData](CancelHistogramData(tickerId = tickerId))
    } yield {
      success
    }

  }
//   test("ibclient can request for current time") { case (ibclient,random) =>
//     for {
//       now <- IO.realTimeInstant
//       serverTime <- ibclient.reqCurrentTime
//     } yield {
//       expect(serverTime.time <= now.getEpochSecond)
//     }
//   }

//   test("ibclient can request family codes") { case (ibclient,random)  =>
//     for {
//       familycodes <- ibclient.reqFamilyCodes
//     } yield {
//       expect(familycodes.familyCodes.length == 1)
//     }
//   }

// //
// //  test("ibclient can request to set server log level") { ibclient =>
// //    for {
// //      _ <- ibclient.setServerLogLevel(1)
// //    } yield {
// //      expect(true)
// //    }
// //  }

//   test("ibclient can request positions") { case (ibclient,random) =>
//     for {
//       positions <- ibclient.reqPositions.compile.toList
//       _ <- ibclient.cancelPositions
//     } yield {

//       expect(positions.nonEmpty)
//     }
//   }

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
