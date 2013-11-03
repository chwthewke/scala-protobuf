package net.chwthewke.scala.protobuf

sealed trait Repeated[C, M] extends Field[C, Vector[C], M] {
  def packed: Boolean

  override def eval(in: Vector[C]): Vector[C] = in
  override def lift(f: Vector[C]): Vector[C] = f

  override def merge(left: Vector[C], right: Vector[C]): Vector[C] = left ++ right

  def <=(ts: Iterable[C]): Update = set(ts.toVector)
  override def <+=(t: C): Update = alter(_ :+ t)
  def <++=(ts: Iterable[C]): Update = alter(_ ++ ts)

}

object Packed {

  sealed trait Packable[T]

  implicit val packableInt: Packable[Int] = new Packable[Int] {}
  implicit val packableLong: Packable[Long] = new Packable[Long] {}
  implicit val packableFloat: Packable[Float] = new Packable[Float] {}
  implicit val packableDouble: Packable[Double] = new Packable[Double] {}
  implicit val packableBoolean: Packable[Boolean] = new Packable[Boolean] {}
  implicit def packableEnum[M <: Numbered: ProtobufEnum]: Packable[M] = new Packable[M] {}

  def apply[C, M](name: String, number: Int, fieldType: FieldType[C], getter: M => Vector[C])(implicit P: Packable[C]) =
    Repeated.mkField(name, number, true, fieldType, getter)

}

object Repeated {

  def apply[C, M](name: String, number: Int, fieldType: FieldType[C], getter: M => Vector[C]) =
    mkField(name, number, false, fieldType, getter)

  private[protobuf] def mkField[C, M](name: String, number: Int, packed: Boolean, fieldType: FieldType[C], getter: M => Vector[C]): Repeated[C, M] = {
    val (n, i, p, f) = (name, number, packed, fieldType)
    new Repeated[C, M] {
      override def name: String = n
      override def number: Int = i
      override def packed: Boolean = p
      override def get(m: M): Vector[C] = getter(m)
      override def fieldType: FieldType[C] = f
    }
  }
}
