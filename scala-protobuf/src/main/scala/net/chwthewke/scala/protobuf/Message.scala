package net.chwthewke.scala.protobuf

import scala.annotation.tailrec
import scala.language.higherKinds

trait Message[M] {

  sealed trait Field {

    self =>

    type F[+_]
    type A

    def name: String
    def number: Int
    def fieldType: FieldType[A]
    def wireType: Int = fieldType.wireType

    def get: M => F[A]

    def lift(value: F[A]): Vector[A]
    def eval(parts: Vector[A]): F[A]

    def values(m: M): Vector[A] = lift(get(m))

    def merge(left: Vector[A], right: Vector[A]): Vector[A]

    private[protobuf] def add(value: A): Update
    private[protobuf] def addAll(values: Vector[A]): Update = Update(merge(_, values))

    @tailrec
    final def build(fieldValues: FieldValues): F[A] = fieldValues match {
      case Nil                                => Value().get
      case (v: Value) :: _ if v.field == this => v.get
      case _ :: vs                            => build(vs)
    }

    object Value {
      def apply(): Value = Value(Vector.empty)
      def apply(m: M): Value = Value(lift(get(m)))
    }

    case class Value(parts: Vector[A]) {
      private[Message] def field = Field.this
      private[Message] def asUpdate: Update = Field.this addAll parts

      def get: F[A] = eval(parts)

      private[Field] def mod(update: Field#Update): Option[Value] = update match {
        case up: Update if Field.this == up.field => Some(Value(up.apply(parts)))
        case _                                    => None
      }
    }

    object Update {
      def apply(f: Vector[self.A] => Vector[self.A]): Update = new Update {
        override def apply(parts: Vector[self.A]): Vector[self.A] = f(parts)
      }
    }

    trait Update extends (Vector[A] => Vector[A]) {
      private[Message] def field = Field.this

      private[Message] def run(values: FieldValues): FieldValues = values match {
        case Nil => List(Value(apply(Vector.empty)))
        case v :: vs => {
          v mod this match {
            case Some(newV) => newV :: vs
            case None       => v :: run(vs)
          }
        }
      }
    }

    def unary_- : Update = Update(_ => Vector.empty)
  }

  sealed trait Singular[T] extends Field {
    override type A = T
    override def merge(left: Vector[T], right: Vector[T]): Vector[T] = (left ++ right) takeRight 1

    def <=(x: T): Update = Update(_ => Vector(x))
    def <|=(x: T): Update = Update(x +: _)

    override private[protobuf] def add(value: T) = <=(value)
  }

  case class Required[T](
    override val name: String,
    override val number: Int,
    override val fieldType: FieldType[T],
    override val get: M => T)
    extends Singular[T] {

    override type F[+A] = A
    override def eval(parts: Vector[T]): T = parts.last
    override def lift(x: T): Vector[T] = Vector(x)
  }

  case class Optional[T](
    override val name: String,
    override val number: Int,
    override val fieldType: FieldType[T],
    override val get: M => Option[T])
    extends Singular[T] {

    override type F[+A] = Option[A]
    override def eval(parts: Vector[T]): Option[T] = parts.lastOption
    override def lift(x: Option[T]): Vector[T] = x.toVector
  }

  sealed trait Multiple[T] extends Field {
    override type A = T
    override type F[+A] = Vector[A]

    override def eval(parts: Vector[T]): Vector[T] = parts
    override def lift(x: Vector[T]): Vector[T] = x
    override def merge(left: Vector[T], right: Vector[T]): Vector[T] = left ++ right

    def <=(xs: Iterable[T]): Update = Update(_ => xs.toVector)
    def <+=(x: T): Update = Update(_ :+ x)
    def <++=(xs: Iterable[T]) = addAll(xs.toVector)

    override private[protobuf] def add(value: T) = <+=(value)
  }

  case class Repeated[T](
    override val name: String,
    override val number: Int,
    override val fieldType: FieldType[T],
    override val get: M => Vector[T])
    extends Multiple[T]

  case class Packed[T](
    override val name: String,
    override val number: Int,
    override val fieldType: FieldType[T],
    override val get: M => Vector[T])
    extends Multiple[T] {
    override def wireType: Int = WireType.LengthPrefixed
  }

  type FieldValues = List[Field#Value]

  case class Builder(values: FieldValues) {
    def build: M = Message.this.build(this)
    def mod(updates: Field#Update*): Builder = Builder((values /: updates) { (v, u) => u run v })
    def eval(f: Field): f.F[f.A] = f.build(values)
    def ++(other: Builder): Builder = mod(other.values map (_.asUpdate): _*)
  }

  object Builder {
    def apply(updates: Field#Update*): Builder = Builder(Nil) mod (updates: _*)
  }

  def build(values: Builder): M
  def fields: Vector[Field]

  def newBuilder(m: M): Builder = Builder(fields.toList map (_.Value(m)))
  def newBuilder: Builder = Builder(Nil)

  def merge(left: M, right: M): M = (newBuilder(left) ++ newBuilder(right)).build
  def mod(m: M, updates: Seq[Field#Update]): M = (newBuilder(m) mod (updates: _*)).build

}
