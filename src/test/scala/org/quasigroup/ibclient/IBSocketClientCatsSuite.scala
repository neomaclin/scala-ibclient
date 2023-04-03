package org.quasigroup.ibclient

import org.quasigroup.ibclient.client.IBClient
import org.quasigroup.ibclient.client.impl.IBSocketClientCats

import cats.effect.{IO, SyncIO}
import cats.effect.kernel.Resource

import weaver.IOSuite
import weaver.scalacheck._
import org.scalacheck.Gen

object IBSocketClientCatsSuite extends IOSuite with Checkers {

  override type Res = IBClient[IO]

  override def sharedResource: Resource[IO, Res] = IBSocketClientCats.make[IO]()

  test("ibclient can connect and receive connection ack with server version and timestamp") { ibclient =>
    ibclient.eConnect(10).map(ack => expect(ack.serverVersion == IBClient.MAX_VERSION))
  }
  
}
