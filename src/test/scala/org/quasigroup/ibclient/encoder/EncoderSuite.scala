package org.quasigroup.ibclient.encoder

import org.quasigroup.ibclient.client.encoder.Encoder
import org.quasigroup.ibclient.client.encoder.Encoder.{encode, given}
import org.quasigroup.ibclient.IBSocketClientCatsSuite.{expect, test}
import org.quasigroup.ibclient.client.request.RequestMsg.StartAPI
import org.quasigroup.ibclient.client.IBClient
import weaver.SimpleIOSuite

import scala.collection.mutable
import org.quasigroup.ibclient.client.types.*

object EncoderSuite extends SimpleIOSuite {

  pureTest("encoder can encode StartAPI") {
    val expected = {
      val encoded =
        Encoder.IntEncoder(71) ++ Encoder.IntEncoder(2) ++ Encoder.IntEncoder(
          10
        ) ++ StringEncoder("")
      val length = Encoder.Length(encoded.length)
      val lengthEncoded = LengthEncoder.apply(length)
      (lengthEncoded ++ encoded).toArray
    }
    val result = encode[StartAPI](StartAPI(clientId = 10))
    expect(expected sameElements result)
  }

  pureTest("encoder can encode simple enum") {

    val result = encode[MktDataType](MktDataType.Delayed)
    println(String.valueOf(result))
    expect(true)
  }
}
