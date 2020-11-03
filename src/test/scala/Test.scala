import java.sql.{Connection, DriverManager}
import java.util.Date

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import anorm._
import ru.dimonz80.{QueryHelper, field, table}

class Test extends AnyWordSpec with should.Matchers with BeforeAndAfterAll {

  val driverName = "org.h2.Driver"
  val dbUrl = "jdbc:h2:mem:/test;MODE=PostgreSQL"

  Class.forName(driverName)

  val conn = DriverManager.getConnection(dbUrl, "", "")


  case class TestEntity(id: Option[Long], name: String, date: Date)


  @table("TESTENTITY")
  case class AlterNamedTestEntity(id: Option[Long],
                                  @field("NAME")
                                  alterName: String,
                                  @field("DATE")
                                  alterDate: Date)


  val now = new Date()

  override def beforeAll(): Unit = {

    SQL"""
        create table TestEntity (
          id bigserial primary key not null,
          name varchar(256) not null,
          date date not null
        );
    """.execute()(conn)

    Range(0, 100).map { id =>
      SQL"""insert into TestEntity(name,date) values (${"name" + id},${now})""".executeUpdate()(conn)
    }
  }


  "Query helper" should {
    val table = QueryHelper[TestEntity]

    val alterTable = QueryHelper[AlterNamedTestEntity]

    "find entity by id" in {
      val id = 1
      table.find(id)(conn).map { entity =>
        entity.id shouldEqual Option(id)
      }.getOrElse {
        fail("Can't find entity")
      }
    }

    "find alternative named entity by id" in {
      val id = 1
      alterTable.find(id)(conn).map { entity =>
        entity.id shouldEqual Option(id)
      }.getOrElse {
        fail("Can't find entity")
      }
    }

    "find entity by parameters" in {
      table.find("id" -> 1)(conn).exists(_.id == Option(1)) shouldEqual true

      table.find("name" -> "name0")(conn).exists(_.name == "name0") shouldEqual true
    }


    "insert some value" in {
      val entity = TestEntity(None, "SOME NAME", new Date())
      val id = table.insert(entity)(conn)
      table.find(id)(conn).map { saved =>
        saved.id shouldEqual Option(id)
        saved.name shouldEqual entity.name
      }.getOrElse {
        fail("can't find saved entity")
      }
    }


    "update entity" in {
      val entity = TestEntity(None, "changed entity", new Date())
      val id = 1
      table.update(id, entity)(conn)
      val saved = table.find(id)(conn)

      saved.get.name shouldEqual entity.name

    }

    "delete entity by id" in {
      table.delete(1)(conn)
      table.find(1)(conn).isEmpty shouldEqual true
    }

    "delete entity by parameters" in {
      table.delete("id" -> 6)(conn)
      table.find("id" -> 6)(conn).isEmpty shouldEqual true
    }

  }

}
