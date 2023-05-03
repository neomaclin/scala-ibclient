package org.quasigroup.ibclient.response.readers

import org.quasigroup.ibclient.IBClient
import org.quasigroup.ibclient.decoder.Decoder.{DecoderState, read, readNothing}
import org.quasigroup.ibclient.response.ResponseMsg.TickOptionComputation
import org.quasigroup.ibclient.types.{TickAttrib, TickType}
import org.quasigroup.ibclient.types.TypesCodec.given
import cats.syntax.eq.*

object TickOptionComputationReader {
  def create(using serverVersion: IBClient.ServerVersion): DecoderState[TickOptionComputation] =
    for
      version <- read[Int]
      tickerId <- read[Int]
      tickType <- read[TickType]
      tickAttrib <- read[TickAttrib]
      impliedVol <- read[Double].map(impliedVol => if impliedVol === -1 then Double.MaxValue else impliedVol)
      delta <- read[Double].map(delta => if delta === -2 then Double.MaxValue else delta)
      isOption =
        (version >= 6 || tickType == TickType.MODEL_OPTION || tickType == TickType.DELAYED_MODEL_OPTION)
      optPrice <-
        if isOption then read[Double].map(optPrice => if optPrice === -1 then Double.MaxValue else optPrice)
        else readNothing(Double.MaxValue)
      pvDividend <-
        if isOption then read[Double].map(pvDividend => if pvDividend === -1 then Double.MaxValue else pvDividend)
        else readNothing(Double.MaxValue)
      isRightVersion = (version >= 6)
      gamma <-
        if isRightVersion then read[Double].map(gamma => if gamma === -2 then Double.MaxValue else gamma)
        else readNothing(Double.MaxValue)
      vega <-
        if isRightVersion then read[Double].map(vega => if vega === -2 then Double.MaxValue else vega)
        else readNothing(Double.MaxValue)
      theta <-
        if isRightVersion then read[Double].map(theta => if theta === -2 then Double.MaxValue else theta)
        else readNothing(Double.MaxValue)
      undPrice <-
        if isRightVersion then read[Double].map(undPrice => if undPrice === -1 then Double.MaxValue else undPrice)
        else readNothing(Double.MaxValue)
    yield TickOptionComputation(
      tickerId,
      tickType,
      tickAttrib,
      impliedVol,
      delta,
      optPrice,
      pvDividend,
      gamma,
      vega,
      theta,
      undPrice
    )
}
