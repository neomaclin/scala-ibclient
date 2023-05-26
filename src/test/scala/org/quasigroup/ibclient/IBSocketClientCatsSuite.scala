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

import scala.language.postfixOps
import scala.concurrent.duration._

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
      //
      // _ <- ibclient.requestOnly[CancelHistogramData](CancelHistogramData(tickerId = tickerId))
      _ <- IO.sleep(60 seconds)
    } yield {
      success
    }
  }

//    test("obtain historical ticks") { case (ibclient, random) =>
//     for {
//       reqId1 <- random.nextInt
//       req1 <- ReqHistoricalTicks(
//         reqId = reqId1,
//         contract = USStockAtSmart,
//         startDateTime = "20220808 10:00:00 US/Eastern",
//         endDateTime = "",
//         numberOfTicks = 10,
//         whatToShow = WhatToShow.TRADES,
//         useRTH = 1,
//         ignoreSize = true,
//         miscOptions = Nil
//       ).pure
//       _ <- ibclient.requestOnly[ReqHistoricalTicks](req1)
//       reqId2 <- random.nextInt
//       _ <- ibclient.requestOnly[ReqHistoricalTicks](req1.copy(reqId = reqId2, whatToShow = WhatToShow.BID_ASK))
//       reqId3 <- random.nextInt
//       _ <- ibclient.requestOnly[ReqHistoricalTicks](req1.copy(reqId = reqId3, whatToShow = WhatToShow.MIDPOINT))
//     } yield {
//       success
//     }
//   }

// test("obtain pnls") { case (ibclient, random) =>
//   for {
//     reqId <- random.nextInt
//     _ <- ibclient.requestOnly[ReqPnL](ReqPnL(reqId = reqId, account = "DUD00029", modelCode = ""))
//     _ <- ibclient.requestOnly[CancelPnL](CancelPnL(reqId = reqId))
//   } yield {
//     success
//   }
// }

// test("obtain pnl single") { case (ibclient, random) =>
//   for {
//     reqId <- random.nextInt
//     _ <- ibclient.requestOnly[ReqPnLSingle](
//       ReqPnLSingle(reqId = reqId, account = "DUD00029", modelCode = "", conId = 268084)
//     )
//     _ <- ibclient.requestOnly[CancelPnLSingle](CancelPnLSingle(reqId = reqId))
//   } yield {
//     success
//   }
// }

//  test("obtain news bulletins") { case (ibclient, random) =>
//    for {
//      // reqId <- random.nextInt
//      ReqNewsBulletins <- ibclient
//        .requestStreamResponse[ReqNewsBulletins, UpdateNewsBulletin](ReqNewsBulletins(allMsgs = true))
//        .take(100)
//        .compile
//        .toList
//      _ <- ibclient.requestOnly[CancelNewsBulletins](CancelNewsBulletins())
//    } yield {
//      success
//    }
//  }

// test("tick Data Operations") { case (ibclient, random) =>
//   for {
//     // reqId <- random.nextInt
//      _ <- ibclient.requestOnly[ReqMarketDataType](ReqMarketDataType(marketDataType = MktDataType.DelayedFrozen))

//   } yield {
//     success
//   }
// }

// test("tick Option Computations") { case (ibclient, random) =>
//   for {
//     // reqId <- random.nextInt
//      _ <- ibclient.requestOnly[ReqMarketDataType](ReqMarketDataType(marketDataType = MktDataType.DelayedFrozen))

//   } yield {
//     success
//   }
// }

// test("realTime Bars") { case (ibclient, random) =>
//   for {
//     // reqId <- random.nextInt
//     tickerId <- random.nextInt
//     _ <- ibclient.requestOnly[ReqRealTimeBars](ReqRealTimeBars(tickerId = tickerId, contract = EurGbpFx, barSize = 5, whatToShow = WhatToShow.MIDPOINT, useRTH = true, realTimeBarsOptions = Nil))
//    // _ <- ibclient.requestOnly[CancelRealTimeBars](CancelRealTimeBars(tickerId = tickerId))

//   } yield {
//     success
//   }
// }

// test("account Operations") { case (ibclient, random) =>
//   for {
//     reqId <- random.nextInt
//     _ <- ibclient.requestOnly[ReqPositionsMulti](
//       ReqPositionsMulti(reqId = reqId, account = "DU74649", modelCode = "EUstocks")
//     )
//     //
//    // _ <- ibclient.requestOnly[CancelHistogramData](CancelHistogramData(tickerId = reqId))
//     accounts <- ibclient.reqManagedAccts
//     familycodes <- ibclient.reqFamilyCodes
//     positions <- ibclient.reqPositions.compile.toList
//             _ <- ibclient.cancelPositions
//   } yield {
//     expect(familycodes.familyCodes.length >= 1) and
//     expect(accounts.accountsList.nonEmpty) and
//     expect(positions.nonEmpty)
//   }
// }

//   test("ibclient can request for current time") { case (ibclient,random) =>
//     for {
//       now <- IO.realTimeInstant
//       serverTime <- ibclient.reqCurrentTime
//     } yield {
//       expect(serverTime.time <= now.getEpochSecond)
//     }
//   }
