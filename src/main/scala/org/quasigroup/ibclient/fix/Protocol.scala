package org.quasigroup.ibclient.fix

sealed trait Protocol {
  def output: String
}
object Protocol {
  val soh: Char = 1.toChar
}
final case class Field(tag: Tag, value: Value) extends Protocol {
  override def output: String = tag.toString + "=" + value.output + Protocol.soh
}

final case class Tag(tag: Int) extends Protocol {
  override def output: String = tag.toString
}

final case class Value(value: String) extends Protocol {
  override def output: String = value
}

final case class Message(fields: Field*) extends Protocol {
  override def output: String = fields.foldLeft("")(_ + _.output)
}

final case class Messages(messages: Message*) extends Protocol {
  override def output: String = messages.map(_.output).mkString("\n")
}

final case class ParseError(message: String) extends Protocol {
  override def output: String = message
}