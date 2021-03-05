package com.evolutiongaming.bootcamp.typeclass


import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner) //implement the syntax!
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      private def scoreSize: SizeScore = map.iterator
        .map { case (k, v) => k.sizeScore + v.sizeScore }
        .sum

      private def canBeAdded(key: K, value: V): Boolean = {
        val newElementSize = key.sizeScore + value.sizeScore

        scoreSize + newElementSize <= maxSizeScore
      }

      def put(key: K, value: V): Unit =
        if (canBeAdded(key, value)) {
          map.addOne(key, value)
        } else {
          if (map.nonEmpty) {
            map.subtractOne(map.head._1)
            put(key, value)
          }
        }

      def get(key: K): Option[V] = map.get(key)

    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      def apply[F[_] : Iterate]: Iterate[F] = implicitly
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      def apply[F[_, _] : Iterate2]: Iterate2[F] = implicitly
    }

    object instances {
      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val setIterate: Iterate[Set] = new Iterate[Set] {
        override def iterator[T](f: Set[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate: Iterate2[mutable.Map] = new Iterate2[mutable.Map] {
        override def iterator1[T, S](f: mutable.Map[T, S]): Iterator[T] = f.keys.iterator

        override def iterator2[T, S](f: mutable.Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val packedMultiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map(_._1).iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map(_._2).iterator
      }

      val BYTE_SIZE = 1
      val CHAR_SIZE = 2
      val INT_SIZE = 4
      val LONG_SIZE = 8
      val OBJECT_HEADER_SIZE = 12

      implicit val byteSizeScore: GetSizeScore[Byte] = _ => BYTE_SIZE
      implicit val charSizeScore: GetSizeScore[Char] = _ => CHAR_SIZE
      implicit val intSizeScore: GetSizeScore[Int] = _ => INT_SIZE
      implicit val longSizeScore: GetSizeScore[Long] = _ => LONG_SIZE

      implicit val StringSizeScore: GetSizeScore[String] = OBJECT_HEADER_SIZE + _.length * BYTE_SIZE
      implicit def ArraySizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = OBJECT_HEADER_SIZE + _.map(_.sizeScore).sum
      implicit def ListSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = OBJECT_HEADER_SIZE + _.map(_.sizeScore).sum
      implicit def VectorSizeScore[T: GetSizeScore]: GetSizeScore[Vector[T]] = OBJECT_HEADER_SIZE + _.map(_.sizeScore).sum
      implicit def IterableSizeScore[T: GetSizeScore, F[_] : Iterate]: GetSizeScore[F[T]] =
        iter => OBJECT_HEADER_SIZE + Iterate[F].iterator(iter).map(_.sizeScore).sum
      implicit def Iterable2SizeScore[K: GetSizeScore, V: GetSizeScore, F[_, _] : Iterate2]: GetSizeScore[F[K, V]] =
        iter => OBJECT_HEADER_SIZE + Iterate2[F].iterator1(iter).map(_.sizeScore).sum + Iterate2[F].iterator2(iter).map(_.sizeScore).sum
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    implicit val fbiNoteSizeScore: GetSizeScore[FbiNote] = fbiNote =>
      OBJECT_HEADER_SIZE + fbiNote.month.sizeScore + fbiNote.favouriteChar.sizeScore + fbiNote.watchedPewDiePieTimes.sizeScore

    implicit val sizeScoreTwit: GetSizeScore[Twit] = twit =>
      OBJECT_HEADER_SIZE + twit.id.sizeScore +twit.userId.sizeScore +twit.hashTags.sizeScore + twit.attributes.sizeScore + twit.fbiNotes.sizeScore

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      def put(twit: Twit): Unit = cache.put(twit.id, twit)

      def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}

