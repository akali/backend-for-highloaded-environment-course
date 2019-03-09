import scala.collection.JavaConversions.asJavaCollection

object Lab4 extends App {
  case class Film( name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)
  case class Director( firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film])


  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)
  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))
  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  // Task 7
  println(mcTiernan.films.minBy(film => film.yearOfRelease).name)


  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  // Task 5
  val nolanFilms = nolan.films.map(film => film.name)

  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())
  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  // Task 10
  directors.foreach(director => {
    director.films.map(film => s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}")
      .foreach(result => {
        println(result)
      })
  })

  // Task 11
  directors.foreach(director => {
    if (director.films.nonEmpty)
      println(s"The earliest film of ${director.firstName} ${director.lastName} is " +
        s"${director.films.minBy(film => film.yearOfRelease).name}")
  })

  // Task 8
  println(directors.flatMap(director => director.films).sortBy(film => film.imdbRating).reverse)

  // Task 9
  val filmRatings = directors.flatMap(director => director.films.map(film => film.imdbRating))
  println(1.0 * filmRatings.sum / filmRatings.length)

  // Task 6
  val cinephile = directors.flatMap(directors => {
    directors.films.map(film => film.name)
  })

  cinephile.foreach(cinephile => {
    println(cinephile)
  })

  def task1(directors: Seq[Director], numberOfFilms: Int): Seq[Director] = {
    directors.filter(director => {
      director.films.length >= numberOfFilms
    })
  }

  def task2(year: Int): Option[Director] = {
    directors.find(director => {
      director.yearOfBirth < year
    })
  }

  def task3(directors: Seq[Director], numberOfFilms: Int, year: Int): Seq[Director] = {
    directors.filter(director => {
      director.yearOfBirth < year && director.films.length >= numberOfFilms
    })
  }

  def task4(directors: Seq[Director], ascending: Boolean): Seq[Director] = {
    directors sortWith((a: Director, b: Director) => {
      ascending ^ a.yearOfBirth >= b.yearOfBirth
    })
  }

  def divider(a: Int, b: Int): Either[String, Int] = {
    try {
      Right(a / b)
    } catch {
      case _: Exception => Left("Could not divide")
    }
  }

  def toInt(s: String): Either[String, Int] = {
    try {
      Right(Integer.parseInt(s.trim))
    } catch {
      case _: Exception => Left("Could not parse int")
    }
  }

  def op(a: Int, b: Int, op: String): Either[String, Int] = {
    op match {
      case "+" => Right(a + b)
      case "-" => Right(a - b)
      case "*" => Right(a * b)
      case "/" => divider(a, b)
      case _ => Left("Could not execute operation")
    }
  }

  def calculator(operand1: String, operator: String, operand2: String) = {
    toInt(operand1).flatMap(a => {
      toInt(operand2).flatMap(b => {
        op(a, b, operator).flatMap(result => {
          Right(result)
        })
      })
    })
  }

  calculator("123", "/", "10") match {
    case Right(x) => println(x)
    case Left(x) => println(x)
  }
}
