package compose.graphql

import zio.Scope
import zio.test._
import zio.test.Assertion._

import caliban.ResponseValue

/**
 * A Test Suite to generate a GraphQL endpoint from REST
 * apis on https://jsonplaceholder.typicode.com
 */
object JsonPlaceholderSpec extends ZIOSpecDefault {
  import compose.graphql.internal._

  def obj(list: (String, ResponseValue)*): ResponseValue = ResponseValue.ObjectValue(list.toList)
  def list(list: ResponseValue*): ResponseValue          = ResponseValue.ListValue(list.toList)

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("JsonPlaceholderSpec")(
    test("query") {
      val graph = JsonPlaceholder.graph

      val query = """
        query {
          users {
            id
            name
          }
        }
      """

      import zio.json._
      import ResponseValue._

      val result = for {
        g <- Executor.make(graph)
        i <- g.interpreter
        _ <- i.check(query)
        r <- i.execute(query)

      } yield r.toJsonPretty

      val expected = """|{
                        |  "data" : {
                        |    "users" : [
                        |      {
                        |        "id" : 1,
                        |        "name" : "Leanne Graham"
                        |      },
                        |      {
                        |        "id" : 2,
                        |        "name" : "Ervin Howell"
                        |      },
                        |      {
                        |        "id" : 3,
                        |        "name" : "Clementine Bauch"
                        |      },
                        |      {
                        |        "id" : 4,
                        |        "name" : "Patricia Lebsack"
                        |      },
                        |      {
                        |        "id" : 5,
                        |        "name" : "Chelsey Dietrich"
                        |      },
                        |      {
                        |        "id" : 6,
                        |        "name" : "Mrs. Dennis Schulist"
                        |      },
                        |      {
                        |        "id" : 7,
                        |        "name" : "Kurtis Weissnat"
                        |      },
                        |      {
                        |        "id" : 8,
                        |        "name" : "Nicholas Runolfsdottir V"
                        |      },
                        |      {
                        |        "id" : 9,
                        |        "name" : "Glenna Reichert"
                        |      },
                        |      {
                        |        "id" : 10,
                        |        "name" : "Clementina DuBuque"
                        |      }
                        |    ]
                        |  }
                        |}""".stripMargin

      assertZIO(result)(equalTo(expected))
    },
    test("schema") {
      val graph = JsonPlaceholder.graph

      val actual = Executor.make(graph).map(_.render)

      val expected = """|schema {
                        |  query: Query
                        |}
                        |
                        |type Address {
                        |  street: String!
                        |  suite: String!
                        |  city: String!
                        |  zipcode: String!
                        |  geo: Geo!
                        |}
                        |
                        |type Album {
                        |  userId: Int!
                        |  id: Int!
                        |  title: String!
                        |  photos: [Photo!]!
                        |  user: User!
                        |}
                        |
                        |type Comment {
                        |  postId: Int!
                        |  id: Int!
                        |  name: String!
                        |  email: String!
                        |  body: String!
                        |}
                        |
                        |type Company {
                        |  name: String!
                        |  catchPhrase: String!
                        |  bs: String!
                        |}
                        |
                        |type Geo {
                        |  lat: String!
                        |  lng: String!
                        |}
                        |
                        |type Photo {
                        |  albumId: Int!
                        |  id: Int!
                        |  title: String!
                        |  url: String!
                        |  thumbnailUrl: String!
                        |  album: Album!
                        |}
                        |
                        |type Post {
                        |  userId: Int!
                        |  id: Int!
                        |  title: String!
                        |  body: String!
                        |  comments: [Comment!]!
                        |  user: User!
                        |}
                        |
                        |type Query {
                        |  posts: [Post!]!
                        |  users: [User!]!
                        |  albums: [Album!]!
                        |}
                        |
                        |type User {
                        |  id: Int!
                        |  name: String!
                        |  username: String!
                        |  email: String!
                        |  address: Address!
                        |  phone: String!
                        |  website: String!
                        |  company: Company!
                        |  albums: [Album!]!
                        |  comments: [Comment!]!
                        |  posts: [Post!]!
                        |}""".stripMargin

      assertZIO(actual)(equalTo(expected))
    },
  )
}
