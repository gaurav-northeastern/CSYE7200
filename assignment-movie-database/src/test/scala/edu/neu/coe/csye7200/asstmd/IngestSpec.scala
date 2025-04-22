package edu.neu.coe.csye7200.asstmd

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.{Codec, Source}
import scala.util._

/**
  * Created by scalaprof on 9/13/16.
  */
class IngestSpec extends AnyFlatSpec with Matchers {

  behavior of "ingest"

  it should "work for Int" in {
    trait ParsableInt$ extends Parsable[Int] {
      def parse(w: String): Try[Int] = Try(w.toInt)
    }
    implicit object ParsableInt$ extends ParsableInt$
    val source = Source.fromChars(Array('x', '\n', '4', '2'))
    val ingester = new Ingest[Int]()
    val xys = ingester(source).toSeq
    // ✅ check that xys has exactly one element, consisting of Success(42)
    xys.size shouldBe 1
    xys.head shouldBe Success(42)
  }

  it should "work for movie database" in {
    implicit val codec: Codec = Codec("UTF-8")
    val source = Source.fromResource("movie_metadata.csv")
    try {
      val ingester = new Ingest[Movie]()
      val mys = for (my <- ingester(source).toList) yield my.recoverWith {
        case e: ParseException => System.err.println(e); my
      }
      val ms = for {
        my <- mys
        m <- my.toOption if m.production.country == "New Zealand"
      } yield m

      ms.size shouldBe 4
      ms foreach println
    } finally {
      source.close()
    }
  }
}
