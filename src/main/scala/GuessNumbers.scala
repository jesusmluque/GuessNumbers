import scala.util.{Failure, Success, Try}

object GuessNumbers {

  trait Monad[F[_]] {
    def pure[A](a: A):F[A]
    def map[A,B](fa: F[A])(f: A => B):F[B]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }
  implicit class monadOps[F[_]: Monad,A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = implicitly[Monad[F]].flatMap(fa)(f)
    def map[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)
    def pure(a: A):F[A] = implicitly[Monad[F]].pure(a)
  }

  case class IO[A](performEff: () => A)
  object IO {def pure[A](a: A): IO[A] = IO(() => a)}
  implicit val ioMonad = new Monad[IO] {
    override def pure[A](a: A): IO[A] = IO.pure(a)

    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = IO(() => f(fa.performEff()))

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(fa.performEff()).performEff())
  }

  def putStrLn(str: String): IO[Unit] = IO(() => println(str))
  def getStrLn: IO[String] = IO(() => readLine)
  def parseInt(str: String): Option[Int] = Try(str.toInt) match {
    case Failure(_) => None
    case Success(value) => Some(value)
  }


  def checkContinue: IO[Boolean] = for {
    _     <- putStrLn("Desea continuar: (y/n) ")
    input <- getStrLn
    cont  <- input match {
              case "y" => IO.pure(true)
              case _ => IO.pure(false)
             }

  } yield cont

  def game(name: String): IO[Unit] = {
    val num = scala.util.Random.nextInt(5) + 1

    for {
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
  }

  def Program: IO[Unit] =
    for {
      _ <-   putStrLn("Nombre: ")
      name <- getStrLn
      _ <- putStrLn(s"Hola $name, bienvenido")
      _ <- game(name)
    } yield ()

  def main(args: Array[String]): Unit = Program.performEff()
}




