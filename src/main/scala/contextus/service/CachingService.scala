package contextus.service

import zio.*
import contextus.model.DomainError

trait Cache[A]:
	def cached[K, R, E](key: K, timeout: Option[Duration] = None)(block: => ZIO[R, E, A]): ZIO[R, E, A]
	def invalidating[K, R, E, T](key: K)(block: => ZIO[R, E, T]): ZIO[R, E, T]

trait CachingService:
	def cache[A]: ZIO[Scope, DomainError.IOError, Cache[A]]

object CachingService:
	
	val live = ZLayer.succeed[CachingService](Live)

	final case class InMemoryCache[A](cacheRef: Ref[Map[Any, A]], scope: Scope) extends Cache[A]:
		override def cached[K, R, E](key: K, timeout: Option[Duration] = None)(block: => ZIO[R, E, A]): ZIO[R, E, A] =
			for {
				valueOpt <- cacheRef.get.map(_.get(key))
				value <- valueOpt.fold(block)(ZIO.succeed)
				_ <- scope.extend(
					ZIO.foreachDiscard(timeout)(d => cacheRef.update(_ - key).delay(d).forkScoped)
				)
			} yield value

		override def invalidating[K, R, E, T](key: K)(block: => ZIO[R, E, T]): ZIO[R, E, T] =
			block.ensuring(cacheRef.update(_ - key))

	object Live extends CachingService:
		override def cache[A]: ZIO[Scope, Nothing, Cache[A]] =
			for {
				scope <- ZIO.service[Scope]
				ref <- Ref.make[Map[Any, A]](Map.empty)
			} yield InMemoryCache(ref, scope)