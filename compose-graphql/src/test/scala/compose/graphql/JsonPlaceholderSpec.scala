package compose.graphql

import compose.graphql.ast.Document
import zio.Scope
import zio.test._

import caliban.RootResolver

/**
 * A Test Suite to generate a GraphQL endpoint from REST
 * apis on https://jsonplaceholder.typicode.com
 */
object JsonPlaceholderSpec extends ZIOSpecDefault {
  import compose.graphql.internal._

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("JsonPlaceholderSpec")(
      test("prototype") {
        val graph = JsonPlaceholder.graph

        implicit val schema = new ASTGenerator(graph).generateSchema

        val resolver = RootResolver(graph)

        val interpreter = caliban.GraphQL.graphQL(resolver)

        val actual = interpreter.render

        val expected =
          """|schema {
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

        assertTrue(actual == expected)
      },
      test("schema") {
        val actual = Document.fromGraph(JsonPlaceholder.graph).render

        val expected =
          """
            |type Address {
            |  city: String!
            |  geo: Geo!
            |  street: String!
            |  suite: String!
            |  zipcode: String!
            |}
            |type Album {
            |  id: Int!
            |  photos: [Photo!]!
            |  title: String!
            |  user: User!
            |  userId: Int!
            |}
            |type Comment {
            |  body: String!
            |  email: String!
            |  id: Int!
            |  name: String!
            |  postId: Int!
            |}
            |type Company {
            |  bs: String!
            |  catchPhrase: String!
            |  name: String!
            |}
            |type Geo {
            |  lat: String!
            |  lng: String!
            |}
            |type Photo {
            |  album: Album!
            |  albumId: Int!
            |  id: Int!
            |  thumbnailUrl: String!
            |  title: String!
            |  url: String!
            |}
            |type Post {
            |  body: String!
            |  comments: [Comment!]!
            |  id: Int!
            |  title: String!
            |  user: User!
            |  userId: Int!
            |}
            |type Query {
            |  albums(userId: Int): [Album!]!
            |  posts: [Post!]!
            |  users: [User!]!
            |}
            |type User {
            |  address: Address!
            |  albums: [Album!]!
            |  comments: [Comment!]!
            |  company: Company!
            |  email: String!
            |  id: Int!
            |  name: String!
            |  phone: String!
            |  posts: [Post!]!
            |  username: String!
            |  website: String!
            |}
            |""".stripMargin
        assertTrue(actual == expected)
      },
    )
}
