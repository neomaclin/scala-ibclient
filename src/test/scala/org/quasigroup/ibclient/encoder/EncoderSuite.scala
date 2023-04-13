package org.quasigroup.ibclient.encoder

import org.quasigroup.ibclient.IBSocketClientCatsSuite.{expect, test}
import org.quasigroup.ibclient.client.IBClient
import org.quasigroup.ibclient.client.encoder.Encoder
import org.quasigroup.ibclient.client.encoder.Encoder.{encode, given}
import org.quasigroup.ibclient.client.request.RequestMsg.StartAPI
import org.quasigroup.ibclient.client.types.*
import weaver.SimpleIOSuite

import scala.collection.mutable

object EncoderSuite extends SimpleIOSuite {

  pureTest("encoder can encode StartAPI") {
    val expected = {
      val encoded =
        IntEncoder(71) ++ IntEncoder(2) ++ IntEncoder(10) ++ summon[Encoder[
          String
        ]]("")
      (LengthEncoder(Encoder.Length(encoded.length)) ++ encoded).toArray
    }
    val result = encode[StartAPI](StartAPI(clientId = 10))
    expect(expected sameElements result)
  }

}
