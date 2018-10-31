package ss

import scala.annotation.tailrec

class SecretSanta {

  def draw(participants: List[String], rules: Map[String, List[String]]): Map[String, String] = {

    @tailrec
    def loop(acc: Map[String, String], curr: Seq[(String, List[String])]): Map[String, String] = {

      if (curr.size == 0) acc
      else {

        val giftGiver = curr.head._1

        val potentialGiftReceivers = scala.util.Random.shuffle(curr.head._2)
        val giftReceiver = potentialGiftReceivers.head

        val updatedTails: Seq[(String, List[String])] = removeFromMultipleLists(curr.tail, giftReceiver)

        loop(acc ++ Map(giftGiver -> giftReceiver), updatedTails)
      }

    }

    val candidates: Seq[(String, List[String])] = createPotentialMatches(participants, rules).sortBy(_._2.length)
    loop(Map(), candidates)
  }

  def createPotentialMatches(lst: List[String], rules: Map[String, List[String]]): Seq[(String, List[String])] = {

    @tailrec
    def loop(acc: Seq[(String, List[String])], curr: List[String]): Seq[(String, List[String])] = {
      if (curr.length == 0) acc
      else {
        val giftGiver: String = curr.head
        val toBeRemoved: List[String] = List(giftGiver) ++ rules.get(giftGiver).toList.flatten

        val assignees: List[String] = removeMoreThanOneElementFromList(lst, toBeRemoved)

        loop(acc ++ Seq((curr.head, assignees)), curr.tail)
      }
    }

    loop(Seq(), lst)
  }

  def removeMoreThanOneElementFromList(data: List[String], valuesToBeRemoved: List[String]): List[String] = {

    @tailrec
    def loop(acc: List[String], curr: List[String]): List[String] = {
      if (curr.length == 0) acc
      else loop(removeByValueFromList(curr.head, acc), curr.tail)
    }

    loop(data, valuesToBeRemoved)
  }

  def removeFromMultipleLists(data: Seq[(String, List[String])], toBeRemoved: String): Seq[(String, List[String])] = {
    def loop(acc: Seq[(String, List[String])], curr: Seq[(String, List[String])]): Seq[(String, List[String])] = {
      if (curr.size == 0) acc
      else loop(acc ++ Seq((curr.head._1, removeByValueFromList(toBeRemoved, curr.head._2))), curr.tail)
    }

    loop(Seq(), data)
  }

  def removeByValueFromList(toBeRemoved: String, lst: List[String]): List[String] = {

    @tailrec
    def loop(acc: List[String], curr: List[String]): List[String] = {
      if (curr.length == 0) acc
      else if (curr.head == toBeRemoved) loop(acc, curr.tail)
      else loop(acc :+ curr.head, curr.tail)

    }

    loop(List(), lst)
  }
}

