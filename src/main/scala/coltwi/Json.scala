//   _____      _             _       _   _______       _ _ _       _     _
//  / ____|    | |           (_)     | | |__   __|     (_) (_)     | |   | |
// | |     ___ | | ___  _ __  _  __ _| |    | |_      ___| |_  __ _| |__ | |_
// | |    / _ \| |/ _ \| '_ \| |/ _` | |    | \ \ /\ / / | | |/ _` | '_ \| __|
// | |___| (_) | | (_) | | | | | (_| | |    | |\ V  V /| | | | (_| | | | | |_
//  \_____\___/|_|\___/|_| |_|_|\__,_|_|    |_| \_/\_/ |_|_|_|\__, |_| |_|\__|
//                                                             __/ |
//                                                            |___/
// A scala implementation of the solo AI for the game
// Colonial Twilight, designed by Brian Train and
// Published by GMT Games
// 
// Copyright (c) 2017 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
                                                                          
package coltwi

import scala.util.Sorting
import scala.util.parsing.combinator._

// Utilities to build and parse Json used when saveing game state

trait JsonSerializable {
  def toJson(): String
}

/**
* An Exception thrown when parsing or building JSON.
*/
class JsonException(reason: String) extends Exception(reason)


private class EscapedStringParser extends JavaTokenParsers {
  override protected val whiteSpace = "".r

  def unicode: Parser[String] = rep1("\\u" ~> """[a-fA-F0-9]{4}""".r) ^^ { stringBytes =>
    new String(stringBytes.map(Integer.valueOf(_, 16).intValue.asInstanceOf[Char]).toArray)
  }

  def escaped: Parser[String] = "\\" ~> """[\\/bfnrt"]""".r ^^ { charStr =>
    val char = charStr match {
      case "r" => '\r'
      case "n" => '\n'
      case "t" => '\t'
      case "b" => '\b'
      case "f" => '\f'
      case x => x.charAt(0)
    }
    char.toString
  }

  def characters: Parser[String] = """[^\"[\x00-\x1F]\\]+""".r // comment to fix emac parsing "

  def string: Parser[String] = "\"" ~> rep(unicode | escaped | characters) <~ "\"" ^^ { list =>
    list.mkString("")
  }

  def parse(s: String) = {
    parseAll(string, s) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new JsonException(x.toString)
      case x @ Error(msg, _) => throw new JsonException(x.toString)
    }
  }
}

/**
* Stolen from the scala book and fixed by making string quotation explicit.
*/
private class JsonParser extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (Map.empty ++ _)

  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] = string ~ ":" ~ value ^^ {
    case name ~ ":" ~ value => (name, value)
  }

  def number: Parser[Any] = floatingPointNumber ^^ {
    case num if num.matches(".*[.eE].*") => BigDecimal(num)
    case num => {
      val rv = num.toLong
      if (rv >= Int.MinValue && rv <= Int.MaxValue) rv.toInt else rv
    }
  }

  lazy val stringParser = (new EscapedStringParser)

  def string: Parser[String] = "\"(\\\\\\\\|\\\\\"|[^\"])*+\"".r ^^ { escapedStr =>
      stringParser.parse(escapedStr)
    }

  def value: Parser[Any] = obj | arr | string | number |
    "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)

  def parse(s: String) = {
    parseAll(value, s) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new JsonException(x.toString)
      case x @ Error(msg, _) => throw new JsonException(x.toString)
    }
  }
}


/**
* Wrapper for the JSON string representation of a data structure. This class exists to
* allow objects to be converted into JSON, attached to another data structure, and not
* re-encoded.
*/
case class JsonQuoted(body: String) {
  override def toString = body
}

/**
* Natively supported scalar types are: Boolean, Int, Long, String.
* Collections are Sequence[T], Map[String, T] where T includes the scalars defined above, or
* recursive Sequence or Map.
*/
object Json {
  /**
  * Quote a string according to "JSON rules".
  */
  def quote(s: String) = {
    def escapedChar(codePoint: Int): String = {
      codePoint match {
        case c if c > 0xffff =>
          val chars = Character.toChars(c)
          "\\u%04x\\u%04x".format(chars(0).toInt, chars(1).toInt)
        case c if c < 0x20 || c > 0x7e => "\\u%04x".format(c.toInt)
        case c => c.toChar.toString
      }
    }
    
    val charCount = s.codePointCount(0, s.length)
    val escaped = for (idx <- 0 until charCount)
      yield s.codePointAt(s.offsetByCodePoints(0, idx)) match {
        case 0x0d => "\\r"
        case 0x0a => "\\n"
        case 0x09 => "\\t"
        case 0x22 => "\\\""
        case 0x5c => "\\\\"
        case 0x2f => "\\/" // to avoid sending "</"
        case c => escapedChar(c)
      }

    "\"%s\"".format(escaped.mkString)
  }

  /**
* Returns a JSON representation of the given object, as a JsonQuoted object.
*/
  def build(obj: Any): JsonQuoted = {
    val rv = obj match {
      case JsonQuoted(body) => body
      case null => "null"
      case x: Boolean => x.toString
      case x: Number => x.toString
      case array: Array[_] => array.map(build(_).body).mkString("[", ",", "]")
      case set: Set[_] => set.toSeq.map(build(_).body).mkString("[", ",", "]")
      case list: Seq[_] => list.map(build(_).body).mkString("[", ",", "]")
      case map: scala.collection.Map[_, _] =>
        map.map { case (k, v) => quote(k.toString) + ":" + build(v).body }.mkString("{", ",", "}")
      case x: JsonSerializable => x.toJson()
      case x =>
        quote(x.toString)
    }
    JsonQuoted(rv)
  }

/**
* Parses a JSON String representation into its native Scala reprsentation.
*/
  def parse(s: String): Any = (new JsonParser).parse(s)
}

