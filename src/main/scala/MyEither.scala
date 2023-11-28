// 参考: https://qiita.com/souchan-t/items/3789e649fe76a8914133

//代数的データ型をenumで定義
enum MyEither [+L, +R]:
  case LeftCase(error: L)
  case RightCase(value: R)

object MyEither :
  def left[L](error: L): MyEither [L, Nothing] = LeftCase(error)

  def right[R](value: R): MyEither [Nothing, R] = RightCase(value)

  //Monad[F[_]]の型パラメータ数と合うようにするため型ラムダを定義
  private type ET2[L] = [R] =>> MyEither [L, R]

  //MyEither のMonadインスタンス
  given[L]: Monad[ET2[L]] with
    override def pure[R](value: R): ET2[L][R] = right(value)

    extension[R, B] (m: ET2[L][R])
      def flatMap(fn: R => ET2[L][B]): ET2[L][B] =
        m match
          case LeftCase(error) => LeftCase(error)
          case RightCase(value) => fn(value)

object MyEitherMain extends App:
  private val right1: MyEither [String, Int] = MyEither .right(1)
  private val right2: MyEither [String, Int] = MyEither .right(2)
  private val left1: MyEither [String, Int] = MyEither .left("error!")
  val result1 = for
    x <- right1
    y <- right2
  yield
    x + y
  println(result1) // RightCase(3)

  val result2 = for
    x <- right1
    _ <- left1
    y <- right2
  yield
    x + y

  println(result2) // LeftCase("error!")
