package module2

object homework_hkt_impllicts{

  /**
    * 
    * Доработать сигнатуру tupleF и реализовать его
    * По итогу должны быть возможны подобные вызовы
    *   val r1 = println(tupleF(optA, optB))
    *   val r2 = println(tupleF(list1, list2))
    * 
    */
  def tupleF[F[_]: Bindable, A, B](fa: F[A], fb: F[B]): F[(A, B)] = fa.flatMap(a => fb.map((a,_)))


  trait Bindable[F[_]] {
      def map[A, B](fa: F[A], f: A => B): F[B]
      def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  }

  implicit class BindableSyntax[F[_]: Bindable, A](fa: F[A]) {
      def map[B](f: A => B)(implicit bindable: Bindable[F]): F[B] = bindable.map(fa, f)
      def flatMap[B](f: A => F[B])(implicit bindable: Bindable[F]): F[B] = bindable.flatMap(fa, f)
  }

  implicit object BindableList extends Bindable[List] {
      override def map[A, B](fa: List[A], f: A => B): List[B] = fa.map(f)
      
      override def flatMap[A, B](fa: List[A], f: A => List[B]): List[B] = fa.flatMap(f)

  }

  implicit object BindableOption extends Bindable[Option] {
      override def map[A, B](fa: Option[A], f: A => B): Option[B] = fa.map(f)
      
      override def flatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = fa.flatMap(f)
      
  }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))
}