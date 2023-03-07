package org.quasigroup.fix

sealed trait Protocol {
  def output: String
}
object Protocol {
  val soh: Char = 1.toChar
}
case class Field(tag: Tag, value: Value) extends Protocol {
  override def output: String = tag.toString + "=" + value.output + Protocol.soh
}

case class Tag(tag: Int) extends Protocol {
  override def output: String = tag.toString
}

case class Value(value: String) extends Protocol {
  override def output: String = value
}

case class Message(fields: Field*) extends Protocol {
  override def output: String = fields.foldLeft("")(_ + _.output)
}

object Message {
  def apply(fields: List[Field]): Message = Message(fields: _*)
}

case class Messages(messages: Message*) extends Protocol {
  override def output: String = messages.map(_.output).mkString("\n")
}

object Messages {
  def apply(messages: List[Message]): Messages = Messages(messages: _*)
}

case class ParseError(message: String) extends Protocol {
  override def output: String = message
}