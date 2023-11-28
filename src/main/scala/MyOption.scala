// 参考: https://qiita.com/souchan-t/items/3789e649fe76a8914133
enum MyOption[+T]:
  case Empty
  case Just(value: T)

  def isEmpty: Boolean = this match
    case Empty => true
    case Just(_) => false

  def getOrElse[A >: T](_else: => A): A =
    this match
      case Empty => _else
      case Just(value) => value

object MyOption:
  def apply[A](value: => A): MyOption[A] =
    if (value != null)
      Just(value)
    else
      Empty

  given Monad[MyOption] with
    override def pure[A](value: A): MyOption[A] = MyOption(value)

    extension[A, B] (m: MyOption[A])
      override def flatMap(fn: A => MyOption[B]): MyOption[B] =
        m match
          case Empty => Empty
          case Just(value) => fn(value)

object MyOptionMain extends App:
  val some1 = MyOption.Just(1)
  val some2 = MyOption.Just(2)
  val none1 = MyOption.Empty
  val result1 =
    for
      r1 <- some1
      r2 <- some1
    yield
      r1 + r2

  println(s"$result1") // Just(2)

  val result2 =
    for
      r1 <- some1
      n1 <- none1
      r2 <- some1
    yield
      r1 + r2

  println(s"$result2") // Empty
