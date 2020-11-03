# Anorm query helper
A simple query generator macro for ```anorm```

## Usage

```scala
import java.util.Date
import ru.dimonz80._


case class Entity(id : Option[Long],name: String, date : Date)

val table = QueryHelper[Entity]

val entity = Entity(None,"NAME", new Date)

implicit val conn : java.sql.Connection = ???
 
val id = table.insert(entity)(conn)

table.find(id)(conn) // => Option[Entity]

table.update(id,entity.copy(name = "Some other name"))(conn)

table.find("name" -> "NAME")(conn) // => Seq[Entity]


```