package org.quasigroup.ibclient.fix

class StandardHeader {
  def toFIX: String = {
    Message(
      Field(Tag(8),Value("FIX.4.3")),
      Field(Tag(9),Value("FIX.4.3")),
      Field(Tag(35),Value("FIX.4.3")),
      Field(Tag(49),Value("FIX.4.3")),
      Field(Tag(56),Value("IB")),
      Field(Tag(57),Value("FIX.4.3")),
      Field(Tag(34),Value("FIX.4.3")),
      Field(Tag(43),Value("FIX.4.3")),
      Field(Tag(97),Value("FIX.4.3")),
      Field(Tag(52),Value("FIX.4.3")),
      Field(Tag(122),Value("FIX.4.3"))
    ).output
  }
}
