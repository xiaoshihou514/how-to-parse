// format: off
//> using scala 3.3.3
//> using platform native
//> using nativeVersion 0.4.17
//> using dep com.github.j-mie6::parsley::4.5.2

import parsley.Parsley
import parsley.character.item
import parsley.combinator.{manyTill, sepBy1}
import parsley.debug._

enum Json:
  case Int(value: BigInt)
  case String(value: java.lang.String)
  case Bool(value: Boolean)
  case Array(value: List[Json])
  case Null()
  case Object(value: List[(java.lang.String, Json)])

object lexer {
  import parsley.token.{Lexer, predicate}
  import parsley.token.descriptions.{LexicalDesc, SpaceDesc}

  private val desc = LexicalDesc.plain.copy(
    spaceDesc = SpaceDesc.plain.copy(
      space = predicate.Basic((c: Char) => c.isWhitespace || c == '\n')
    )
  )

  private val lexer = new Lexer(desc)

  val number = lexer.lexeme.natural.decimal
  val implicits = lexer.lexeme.symbol.implicits
}

import lexer.number
import lexer.implicits._

val str = "\"" ~> manyTill(item, "\"").map(_.mkString)

val int = number.map(Json.Int.apply)

// escaping is not implemented for rust parser as well :)
val string = str.map(Json.String.apply)

val bool = ("true" as Json.Bool(true)) <|> ("false" as Json.Bool(false))

val nil = "null" as Json.Null()

lazy val array: Parsley[Json] = "[" ~> (
  (sepBy1(any, ",").map(Json.Array.apply) <~ "]") <|>
  "]" as Json.Array(Nil)
)

val any = int <|> string <|> bool <|> array <|> nil <|> json

lazy val json: Parsley[Json] = "{" ~> (
  (sepBy1(mappings, ",").map(Json.Object.apply) <~ "}") <|>
  ("}" as Json .Object(Nil))
)

val mappings = (str <~ ":") <~> any

import scala.io.Source
import java.nio.file.{Files, Paths}

@main def main() = {
  val filePath = "test.json"
  val fileBytes =
    Files.size(Paths.get(filePath)) / 1e6
  val input = Source.fromFile(filePath).mkString

  for (n <- 1 to 100) {
    val startTime = System.nanoTime()
    val parseResult = json.parse(input)
    val endTime = System.nanoTime()

    assert(parseResult.isSuccess)
    val durationSeconds = (endTime - startTime) / 1e9
    val mbps = fileBytes / durationSeconds
    println(
      f"Parsing successful! Processed $fileBytes Mb in $durationSeconds%.3f seconds ($mbps%.3f Mb/s)."
    )
  }
}
