package compose.graphql

import compose.graphql.NodePrinter
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

/**
 * A Test Suite to generate a GraphQL endpoint from REST
 * apis on https://jsonplaceholder.typicode.com
 */
object JsonPlaceholderSpec extends ZIOSpecDefault {
  import compose.graphql.internal._

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("JsonPlaceholderSpec")(test("schema") {
      val actual   = NodePrinter.render(JsonPlaceholder.ast)
      val expected = """
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
    })
}
