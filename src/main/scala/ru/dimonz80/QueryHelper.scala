package ru.dimonz80


import anorm.{NamedParameter, Row, SimpleSql}

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

/**
  * Helper for DAO <br/>
  * Usage
  * {{{
  *   case class Person(id : Option[Long], name : String, birthDate : Date)
  *   val personDAO = QueryHelper[Person](None)
  *   db.withConnection { implicit conn =>
  *     val person = personDAO.find(1)
  *     personDAO.update(person.id.get, person.copy(name = "VASYA"))
  *     personDAO.update(person.copy(name = "VASYA"), "id" -> person.id.get)
  *
  *    }
  *
  * }}}
  */
trait QueryHelper[T] {

  /**
    * Insert entity returning PK as Long
    *
    * @param t : T - Inserting entity
    * @param c : Connection
    * @return pk : Long
    */
  def insert(t: T)(implicit c: java.sql.Connection): Long

  /**
    * Construct insert expression for execution with returning user defined PK
    *
    * @param t
    * @param c
    * @return
    */
  def insertExpr(t: T)(implicit c: java.sql.Connection): SimpleSql[Row]

  /**
    * update entity by id : Long
    *
    * @param id
    * @param t
    * @param c
    * @return
    */
  def update(id: Long, t: T)(implicit c: java.sql.Connection): Long

  /**
    * Update entity with user's consitions
    *
    * @param t     - Entity
    * @param where : NamedParameter* = conditions
    * @param conn  - implicit db connection
    * @return
    */
  def update(t: T, where: NamedParameter*)(implicit conn: java.sql.Connection): Long

  /**
    * find entity by Id : Long
    *
    * @param id : Long - PK
    * @param c
    * @return Option[T] - maybe Entity
    */
  def find(id: Long)(implicit c: java.sql.Connection): Option[T]

  /**
    * Find entity with user's consitions
    *
    * @param where - user's conditions
    * @param c     - db connection
    * @return - maybe entity
    */
  def find(where: NamedParameter*)(implicit c: java.sql.Connection): Seq[T]

  /**
    * delete entity by id : Long
    *
    * @param id
    * @param c
    * @return
    */
  def delete(id: Long)(implicit c: java.sql.Connection): Long

  /**
    * delete entities with user's consditions
    *
    * @param where - user's condisions
    * @param c
    * @return
    */
  def delete(where: NamedParameter*)(implicit c: java.sql.Connection): Long

  /**
    * Row parser
    */
  val parser: anorm.RowParser[T]

  def toMap(t: T): Map[String, Any]

  def fromMap(map: Map[String, Any]): T

}

/**
  * Macro magic for constructing QueryHelper[T]
  *
  */
object QueryHelper {

  implicit def apply[T]: QueryHelper[T] = macro applyImpl[T]

  def applyImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[QueryHelper[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head.filter(p => !p.annotations.exists { a => a.tree.tpe.<:<(weakTypeOf[ru.dimonz80.ignored]) }).map { f =>
      f -> f.annotations.find(a => a.tree.tpe.<:<(weakTypeOf[ru.dimonz80.field])).map(_.tree.children.tail.head.toString)
    }

    val (toMapParams, fromMapParams) = fields.map { case (field, fieldName) =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature

      (q"$key -> t.$name", q"map($key).asInstanceOf[$returnType]")
    }.unzip

    val tableNameSymbol = tpe.typeSymbol.annotations
      .find(a => a.tree.tpe.<:<(weakTypeOf[ru.dimonz80.table]))
      .map(p => p.tree.children.tail.head).getOrElse {
      q"${tpe.typeSymbol.name.toString}"
    }

    val schemaNameSymbol = tpe.typeSymbol.annotations
      .find(a => a.tree.tpe.<:<(weakTypeOf[ru.dimonz80.schema]))
      .map(p => p.tree.children.tail.head)

    val tableName = schemaNameSymbol.map { schemaName =>
      q"""$schemaName + "." + $tableNameSymbol"""
    }.getOrElse {
      q"$tableNameSymbol"
    }

    case class Field(name: String, tableFieldName: String)

    val fieldNames = fields.map { case (field, tableFieldName) =>
      Field(field.asTerm.name.toString, tableFieldName.getOrElse(field.asTerm.name.toString))
    }

    val allFields = q"( ..${fieldNames.map(_.tableFieldName).mkString(", ")} )"
    val record = q"""(..${fieldNames.map(_.tableFieldName).filter(_ != "id").mkString(", ")})"""
    val paramsMask = q"""(..${fieldNames.map(_.name).filter(_ != "id").map(f => "{" + f + "}").mkString(",")})"""
    val paramsPairs = q"""(..${fieldNames.filter(_.name != "id").map(f => f.tableFieldName + " = {" + f.name + "}").mkString(",")})"""

    val constructorParams = fields.map { case (field, _) =>
      q"${field.asTerm.name}"
    }

    def makeParser(fields: List[(c.universe.Symbol, Option[String])]): Tree = {
      def makeParser1(fields: List[(c.universe.Symbol, Option[String])]): Tree = {
        fields match {
          case Nil => q"""throw new RuntimeException("Empty field list")"""
          case (head, fieldName) :: Nil => q"""
            get[${head.typeSignature}]($tableNameSymbol + "." + ${fieldName.map(_.replace("\"", "")).getOrElse(head.name.toString)}).map((${head.asTerm.name} : ${head.typeSignature}) =>
                new $tpe(..$constructorParams))"""
          case (head, fieldName) :: tail => q"""get[${head.typeSignature}]($tableNameSymbol + "." + ${fieldName.map(_.replace("\"", "")).getOrElse(head.name.toString)}).flatMap((${head.asTerm.name} : ${head.typeSignature}) => ${makeParser1(tail)})"""
        }
      }

      makeParser1(fields)
    }

    val anormParser = q"${makeParser(fields)}"


    val expr =
      q"""
      import anorm._
      import anorm.SqlParser._

      new QueryHelper[$tpe] {

        val parser = $anormParser

        def insertExpr(t : $tpe)(implicit c : java.sql.Connection) =  {
           val q = "insert into " +    ..$tableName  + "(" +  ..$record + ") values (" + ..$paramsMask +  ")"
           anorm.SQL(q ).on(..$toMapParams)
        }

        def insert(t : $tpe)(implicit c : java.sql.Connection) : Long = {
            insertExpr(t).executeInsert(scalar[Long] single)
        }


        def update(id : Long, t : $tpe)(implicit c : java.sql.Connection) : Long = {
           val str = "update " +  ..$tableName  + " set " + ..$paramsPairs +  " where id = {id}"
           anorm.SQL(str).on(..$toMapParams, 'id -> id).executeUpdate
        }


        def update(t : $tpe, where : NamedParameter*)(implicit c : java.sql.Connection) : Long = {
          val whereExpr = where.map{ p => p.name + " = {" + p.name + "}" }.mkString(", ")
          val sql = "update " +  ..$tableName  + " set " + ..$paramsPairs + " where " +  whereExpr
          anorm.SQL(sql).on(..$toMapParams).on(where : _*).executeUpdate
        }

        def find(id : Long)(implicit c : java.sql.Connection) = {
           val q = "select " + ..$allFields  + " from  " +  ..$tableName + " where id = {id} "
           anorm.SQL(q).on('id -> id).as(parser singleOpt)
        }

        def find(where : NamedParameter*)(implicit c : java.sql.Connection) = {
           val whereExpr = where.map{ p => p.name + " = {" + p.name + "}" }.mkString(" and ")
           val q = "select " + ..$allFields  + " from  " +  ..$tableName + " where  " + whereExpr
           anorm.SQL(q).on(where : _*).as(parser *)
        }


        def delete(id : Long)(implicit c : java.sql.Connection) = {
           val q = "delete from  " +  ..$tableName  + " where id = {id}"
           anorm.SQL(q).on('id -> id).executeUpdate
        }

        def delete(where : NamedParameter*)(implicit conn : java.sql.Connection) = {
          val whereExpr = where.map{ p => p.name + " = {" + p.name + "}" }.mkString(" and ")
          val q = "delete from  " +  ..$tableName  + " where " + whereExpr
          anorm.SQL(q).on(where : _*).executeUpdate
        }

        def toMap(t: $tpe): Map[String, Any] = Map(..$toMapParams)
        def fromMap(map: Map[String, Any]): $tpe = $companion(..$fromMapParams)

      }
    """

    c.Expr[QueryHelper[T]](expr)
  }

}

/**
  * Annotation for assign table name
  *
  * @param name
  */
final class table(name: String) extends StaticAnnotation

/**
  * Annotation for assign schema
  *
  * @param name
  */
final class schema(name: String) extends StaticAnnotation


/**
  * Annotation for override field name
  *
  * @param name
  */
final class field(name: String) extends StaticAnnotation

/**
  * Annotation for ignoring field
  */
final class ignored extends StaticAnnotation

