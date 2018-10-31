package ss

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers, Tag}


class SecretSantaSpec extends FlatSpec with Matchers with PropertyChecks {

  val SS = new SecretSanta()

  "removeByValueFromList" should "remove a given String from a List" taggedAs Tag("UnitTest") in {

    val testData: Seq[((String, List[String]), List[String])] = Seq(
      (("test1", List("test0", "test1", "test2", "test3")), List("test0", "test2", "test3")),
      (("not_present", List("test0", "test1", "test2", "test3")), List("test0", "test1", "test2", "test3")),
      (("test1", List("test1")), List()),
      (("test1", List("test1", "test2", "test3", "test1")), List("test2", "test3"))
    )

    for (i <- testData) {
      val valueToBeRemoved: String = i._1._1
      val listToRemoveValueFrom: List[String] = i._1._2

      val actual = SS.removeByValueFromList(valueToBeRemoved, listToRemoveValueFrom)
      val expected = i._2

      expected shouldBe actual
    }
  }


  "removeFromMultipleLists" should "remove a given String from more than one List" taggedAs Tag("UnitTest") in {

    val testData: Seq[((Seq[(String, List[String])], String), Seq[(String, List[String])])] = Seq(
      (
        (Seq(
          ("Person1", List("Person2", "Person3", "Person4")),
          ("Person2", List("Person1", "Person3", "Person4")),
          ("Person3", List("Person1", "Person2", "Person4")),
          ("Person4", List("Person1", "Person2", "Person3"))
        ), "Person1"),
        Seq(
          ("Person1", List("Person2", "Person3", "Person4")),
          ("Person2", List("Person3", "Person4")),
          ("Person3", List("Person2", "Person4")),
          ("Person4", List("Person2", "Person3"))
        )
      ),
      (
        (Seq(
          ("Person1", List("Person2", "Person3", "Person4")),
          ("Person2", List("Person1", "Person3", "Person4")),
          ("Person3", List("Person1", "Person2", "Person4")),
          ("Person4", List("Person1", "Person2", "Person3"))
        ), "not_present"),
        Seq(
          ("Person1", List("Person2", "Person3", "Person4")),
          ("Person2", List("Person1", "Person3", "Person4")),
          ("Person3", List("Person1", "Person2", "Person4")),
          ("Person4", List("Person1", "Person2", "Person3"))
        )
      ),
      (
        (Seq(
          ("Person1", List("Person1")),
          ("Person2", List("Person1")),
          ("Person3", List("Person1")),
          ("Person4", List("Person1"))
        ), "Person1"),
        Seq(
          ("Person1", List()),
          ("Person2", List()),
          ("Person3", List()),
          ("Person4", List())
        )
      ),
      (
        (Seq(
          ("Person2", List("Person1", "Person3", "Person4", "Person1")),
          ("Person3", List("Person1", "Person2", "Person4", "Person1")),
          ("Person4", List("Person1", "Person2", "Person3", "Person1"))
        ), "Person1"),
        Seq(
          ("Person2", List("Person3", "Person4")),
          ("Person3", List("Person2", "Person4")),
          ("Person4", List("Person2", "Person3"))
        )
      )
    )

    for (i <- testData) {
      val d: Seq[(String, List[String])] = i._1._1
      val valueToBeRemoved: String = i._1._2

      val actual = SS.removeFromMultipleLists(d, valueToBeRemoved)
      val expected = i._2

      expected shouldBe actual
    }

  }

  "removeMoreThanOneElementFromList" should "remove a List of Strings from a List" taggedAs Tag("UnitTest") in {
    val testData: Seq[((List[String], List[String]), List[String])] = Seq(
      ((List("a", "b", "c", "d", "e", "f"), List("a", "b")), List("c", "d", "e", "f")),
      ((List("c", "b", "a"), List("a", "b", "c")), List()),
      ((List("c", "b", "a"), List("d")), List("c", "b", "a")),
      ((List("d", "c", "b", "d", "a", "s", "d", "f", "g"), List("d")), List("c", "b", "a", "s", "f", "g"))
    )

    for (i <- testData) {
      val expected = i._2
      val actual = SS.removeMoreThanOneElementFromList(i._1._1, i._1._2)

      expected shouldBe actual
    }
  }

  "createPotentialMatches" should "take a List of 'Participants' and List of rules and return a Map of String to List of Strings" taggedAs Tag("UnitTest") in {

    val testData = Seq(
      (
        (List("test1", "test2", "test3"), Map("" -> List(""))),
        Seq(("test1" -> List("test2", "test3")),
          ("test2" -> List("test1", "test3")),
          ("test3" -> List("test1", "test2"))
        )
      ),
      (
        (List("test1", "test2", "test3"), Map("test1" -> List("test2"), "test2" -> List("test3"))),
        Seq(("test1" -> List("test3")),
          ("test2" -> List("test1")),
          ("test3" -> List("test1", "test2"))
        )
      ),
      (
        (List("test1", "test2", "test3"), Map("test1" -> List("test4"), "test2" -> List("test3"))),
        Seq(("test1" -> List("test2", "test3")),
          ("test2" -> List("test1")),
          ("test3" -> List("test1", "test2"))
        )
      )
    )

    for (i <- testData) {
      val participants: List[String] = i._1._1
      val rules: Map[String, List[String]] = i._1._2

      val expected: Seq[(String, List[String])] = i._2
      val actual: Seq[(String, List[String])] = SS.createPotentialMatches(participants, rules)

      actual shouldBe expected
    }
  }

  "draw" should "take a Seq of Strings and List of Strings and randomly assign one String to another" taggedAs Tag("UnitTest") in {

    val testData: Seq[((List[String], Map[String, List[String]]), Map[String, String])] = Seq(
      ((List("test1", "test2", "test3"),
        Map("test1" -> List("test3"),
          "test2" -> List("test1"),
          "test3" -> List("test2")
        )), Map("test1" -> "test2", "test2" -> "test3", "test3" -> "test1"))
    )

    for (i <- testData) {
      val expected = i._2

      val actual = SS.draw(i._1._1, i._1._2)

      actual shouldBe expected
    }
  }

}