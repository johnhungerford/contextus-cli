package contextus.model.types

type NonEmptyList[+T] = NonEmptyList.Type[T]

object NonEmptyList:
	opaque type Type[+T] <: List[T] = List[T]
	
	def apply[T](value: T, otherValues: T*): NonEmptyList[T] =
		value +: otherValues.toList
		
	def apply[T](value: T, otherValues: List[T]): NonEmptyList[T] =
		value :: otherValues
		
	def parse[T](list: List[T]): Either[String, NonEmptyList[T]] = list match
		case head :: next => Right(apply(head, next))
		case Nil => Left("NonEmptyList cannot be empty")

	extension [T] (nel: NonEmptyList[T]) 
		def safeLast: T = nel.last

