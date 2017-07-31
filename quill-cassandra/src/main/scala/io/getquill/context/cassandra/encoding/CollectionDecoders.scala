package io.getquill.context.cassandra.encoding

import io.getquill.context.cassandra.CassandraSessionContext
import io.getquill.context.cassandra.util.ClassTagConversions.asClassOf

import scala.reflect.ClassTag
import scala.collection.JavaConverters._

trait CollectionDecoders {
  this: CassandraSessionContext[_] =>

  implicit def listDecoder[T, Cas: ClassTag](implicit mapper: CassandraMapper[Cas, T]): RawDecoder[List[T]] =
    rawDecoder(row => index => row.getList[Cas](index, asClassOf[Cas]).asScala.map(mapper.f).toList)

  implicit def setDecoder[T, Cas: ClassTag](implicit mapper: CassandraMapper[Cas, T]): RawDecoder[Set[T]] =
    rawDecoder(row => index => row.getSet[Cas](index, asClassOf[Cas]).asScala.map(mapper.f).toSet)

  implicit def mapDecoder[K, V, KCas: ClassTag, VCas: ClassTag](
    implicit
    keyMapper: CassandraMapper[KCas, K],
    valMapper: CassandraMapper[VCas, V]
  ): RawDecoder[Map[K, V]] = rawDecoder(row => index => row.getMap[KCas, VCas](index, asClassOf[KCas], asClassOf[VCas])
    .asScala.map(kv => keyMapper.f(kv._1) -> valMapper.f(kv._2)).toMap)

}
