package ss.dev

import ss.SecretSanta


object DevAppRunner extends App {

  val ss = new SecretSanta()

  val participants: List[String] = List("Manus", "Declan", "Cieran", "Rebecca", "Emma", "Adam", "Dylan")
  val rules: Map[String, List[String]] =
    Map("Manus" -> List("Declan"),
      "Declan" -> List("Manus", "Cieran", "Rebecca", "Emma", "Dylan"))


  val results = ss.draw(participants, rules)

  for(i <- results){
    println(s"${i._1} has been assigned to ${i._2}")
  }

}
