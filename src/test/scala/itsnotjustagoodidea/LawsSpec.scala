package itsnotjustagoodidea // It's the law!

import org.scalatest._
import org.scalacheck._

abstract class LawsSpec extends PropSpec with prop.Checkers {

  def checkAll(prefix: String, props: Properties) {
    for (((name, prop), idx) <- props.properties.zipWithIndex) {
      property(prefix + idx + ": " + name) { check(prop) }
    }
  }
}

