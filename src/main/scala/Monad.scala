// 参考: https://qiita.com/souchan-t/items/3789e649fe76a8914133

trait Functor[F[_]]:
  extension[A, B] (m: F[A])
    def map(fn: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def pure[A](x: A): F[A]

  extension[A, B] (m: F[A])
    def flatMap(fn: A => F[B]): F[B]
    //mapのデフォルト実装
    override def map(fn: A => B): F[B] = flatMap(x => pure(fn(x)))