import scala.util.{Failure, Success, Try}

object GuessNumbersTaglessfinal {

  trait Monad[F[_]] {
    def pure[A](a: A):F[A]
    def map[A,B](fa: F[A])(f: A => B):F[B]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }
  implicit class monadOps[F[_]: Monad,A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = implicitly[Monad[F]].flatMap(fa)(f)
    def map[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)

  }
  object Monad {
    def pure[F[_]: Monad, A](a: A):F[A] = implicitly[Monad[F]].pure(a)
  }

  trait Console[F[_]] {
    def putStrLn(str: String): F[Unit]
    def getStrLn: F[String]
  }

  def putStrLn[F[_]: Console](str: String) = implicitly[Console[F]].putStrLn(str)
  def getStrLn[F[_]: Console] = implicitly[Console[F]].getStrLn

  trait RandomNum[F[_]] {
    def nextInt(l: Int): F[Int]
  }
  def nextInt[F[_]: RandomNum](l: Int) = implicitly[RandomNum[F]].nextInt(l)

  def parseInt(str: String): Option[Int] = Try(str.toInt) match {
    case Failure(_) => None
    case Success(value) => Some(value)
  }

  def checkContinue[F[_]: Monad: Console]: F[Boolean] = for {
    _     <- putStrLn("Desea continuar: (y/n) ")
    input <- getStrLn
    cont  <- input match {
      case "y" => Monad.pure(true)
      case _ => Monad.pure(false)
    }

  } yield cont

  def game[F[_]: Monad: RandomNum: Console](name: String): F[Unit] = for {
    num <- nextInt(5)
    _ <- putStrLn("Di un numero: ")
    guess <- getStrLn
    _ <- parseInt(guess) match {
      case None => putStrLn("Lo siento no has introducido un número")
      case Some(value) if value == num => putStrLn(s"Felicidades $name, el número $value es correcto")
      case Some(_) => putStrLn(s"Lo siento $name, no es correcto")
    }
    cont <- checkContinue
    _ <- if (cont) game(name) else putStrLn("Gracias por jugar, adios")

  } yield ()

  def Program[F[_]: Monad: RandomNum: Console]: F[Unit] =
    for {
      _ <-   putStrLn("Nombre: ")
      name <- getStrLn
      _ <- putStrLn(s"Hola $name, bienvenido")
      _ <- game(name)
    } yield ()



  case class IO[A](performEff: () => A)
  object IO {
    def pure[A](a: A): IO[A] = IO(() => a)
    implicit val ioMonad = new Monad[IO] {
      override def pure[A](a: A): IO[A] = IO.pure(a)

      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = IO(() => f(fa.performEff()))

      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(fa.performEff()).performEff())
    }
    implicit val consoleIO = new Console[IO] {
      override def putStrLn(str: String): IO[Unit] = IO(() => println(str))

      override def getStrLn: IO[String] = IO(() => readLine())
    }
    implicit val randomIO = new RandomNum[IO] {
      override def nextInt(l: Int): IO[Int] = IO(() => scala.util.Random.nextInt(l) + 1)
    }
  }
  def main(args: Array[String]): Unit = Program[IO].performEff()
}