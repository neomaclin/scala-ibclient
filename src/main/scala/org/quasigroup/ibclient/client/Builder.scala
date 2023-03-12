package org.quasigroup.ibclient.client

import org.quasigroup.ibclient.client.request.RequestMsg

object Builder {
  private val SEP = 0
  private val PADDING_SIZE = 1 // 1 disables padding, 4 is normal if padding is used

  private val EMPTY_LENGTH_HEADER = new Array[Byte](4)
}

trait Builder[T <: RequestMsg]:
  extension(msg: T) def byteMsg(using serverVersion: Int): Array[Byte]
