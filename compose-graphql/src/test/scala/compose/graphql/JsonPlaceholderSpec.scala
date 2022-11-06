package compose.graphql

import compose.graphql.{AstPrinter}
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
      val actual   = AstPrinter.render(JsonPlaceholder.ast)
      val expected = """
                       |type JsonPlaceholderAddress {
                       |  city: String!
                       |  geo: JsonPlaceholderGeo!
                       |  street: String!
                       |  suite: String!
                       |  zipcode: String!
                       |}
                       |type JsonPlaceholderAlbum {
                       |  id: Int!
                       |  photos: [JsonPlaceholderPhoto!]!
                       |  title: String!
                       |  user: JsonPlaceholderUser!
                       |  userId: Int!
                       |}
                       |type JsonPlaceholderComment {
                       |  body: String!
                       |  email: String!
                       |  id: Int!
                       |  name: String!
                       |  postId: Int!
                       |}
                       |type JsonPlaceholderCompany {
                       |  bs: String!
                       |  catchPhrase: String!
                       |  name: String!
                       |}
                       |type JsonPlaceholderGeo {
                       |  lat: String!
                       |  lng: String!
                       |}
                       |type JsonPlaceholderPhoto {
                       |  album: JsonPlaceholderAlbum!
                       |  albumId: Int!
                       |  id: Int!
                       |  thumbnailUrl: String!
                       |  title: String!
                       |  url: String!
                       |}
                       |type JsonPlaceholderPost {
                       |  body: String!
                       |  comments: [JsonPlaceholderComment!]!
                       |  id: Int!
                       |  title: String!
                       |  user: JsonPlaceholderUser!
                       |  userId: Int!
                       |}
                       |type JsonPlaceholderUser {
                       |  address: JsonPlaceholderAddress!
                       |  albums: [JsonPlaceholderAlbum!]!
                       |  comments: [JsonPlaceholderComment!]!
                       |  company: JsonPlaceholderCompany!
                       |  email: String!
                       |  id: Int!
                       |  name: String!
                       |  phone: String!
                       |  posts: [JsonPlaceholderPost!]!
                       |  username: String!
                       |  website: String!
                       |}
                       |type Query {
                       |  posts: [JsonPlaceholderPost!]!
                       |  users: [JsonPlaceholderUser!]!
                       |}
                       |""".stripMargin
      assertTrue(actual == expected)
    })
}
