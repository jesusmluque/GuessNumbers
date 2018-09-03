import GuessNumbersTaglessfinal.{Console, Monad, Program, RandomNum}

import org.scalatest._


class GuessNumberSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  case class TestData(input: List[String], output: List[String], random: List[Int]) {
    self =>
    def putStrLn(line: String): (TestData, Unit) = (copy(output = line :: self.output), ())

    def getStrLn: (TestData, String) = (copy(input = self.input.tail), self.input.head)

    def nextInt: (TestData, Int) = (copy(random = self.random.tail), self.random.head)
  }
  case class TestIO[A](performEff: TestData => (TestData, A))
  object TestIO {
    def pure[A](a: A): TestIO[A] = TestIO(t => (t, a))

    implicit val ioMonad = new Monad[TestIO] {
      override def pure[A](a: A): TestIO[A] = TestIO.pure(a)

      override def map[A, B](fa: TestIO[A])(f: A => B): TestIO[B] = TestIO((t) => fa.performEff(t) match {
        case (nt, a) => (nt, f(a))
      })

      override def flatMap[A, B](fa: TestIO[A])(f: A => TestIO[B]): TestIO[B] = TestIO((t) => fa.performEff(t)
      match {
        case (nt, a) => f(a).performEff(nt)
      })
    }
    implicit val consoleIO = new Console[TestIO] {
      override def putStrLn(str: String): TestIO[Unit] = TestIO(t => t.putStrLn(str))

      override def getStrLn: TestIO[String] = TestIO(t => t.getStrLn)
    }
    implicit val randomIO = new RandomNum[TestIO] {
      override def nextInt(l: Int): TestIO[Int] = TestIO(t => t.nextInt)
    }
  }

  "The guess number game" should "challenges the player to guess a number and check if it is correct" in {
      val result = Program[TestIO].performEff(TestData(List("jesus", "3", "n"), List(), List(3)))
      result._1.output should be (List("Gracias por jugar, adios", "Desea continuar: (y/n) ", "Felicidades jesus, el número 3 es correcto", "Di un numero: ", "Hola jesus, bienvenido", "Nombre: "))
    }

  "The guess number game" should "inform the player if the guess is not correct" in {
    val result = Program[TestIO].performEff(TestData(List("jesus", "3", "n"), List(), List(2)))
    result._1.output should be (List("Gracias por jugar, adios", "Desea continuar: (y/n) ", "Lo siento jesus, no es correcto", "Di un numero: ", "Hola jesus, bienvenido", "Nombre: "))
  }

  "The guess number game" should "offer to the player the option to repeat" in {
    val result = Program[TestIO].performEff(TestData(List("jesus", "3", "y", "3", "n"), List(), List(2, 3)))
    result._1.output should be (List("Gracias por jugar, adios", "Desea continuar: (y/n) ", "Felicidades jesus, el número 3 es correcto", "Di un numero: ", "Desea continuar: (y/n) ", "Lo siento jesus, no es correcto", "Di un numero: ", "Hola jesus, bienvenido", "Nombre: "))
  }
}
