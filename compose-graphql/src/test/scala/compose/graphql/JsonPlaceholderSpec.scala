package compose.graphql

import compose.graphql.ast.{AstGenerator, AstPrinter}
import compose.{Lambda, ~>}
import zio.Scope
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertTrue}

/**
 * A Test Suite to generate a GraphQL endpoint from REST
 * apis on https://jsonplaceholder.typicode.com
 */
object JsonPlaceholderSpec extends ZIOSpecDefault {
  import compose.graphql.internal._

  object fetch {
    def users: Any ~> List[User]            = Lambda.die
    def posts: Any ~> List[Post]            = Lambda.die
    def postUser: Post ~> User              = Lambda.die
    def userPosts: User ~> List[Post]       = Lambda.die
    def userAlbums: User ~> List[Album]     = Lambda.die
    def albumPhotos: Album ~> List[Photo]   = Lambda.die
    def postComments: Post ~> List[Comment] = Lambda.die
    def albumUser: Album ~> User            = Lambda.die
    def userComments: User ~> List[Comment] = Lambda.die
    def photoAlbum: Photo ~> Album          = Lambda.die
  }

  override def spec
    : Spec[TestEnvironment with Scope, Any] = suite("JsonPlaceholderSpec")(test("schema") {

    val connection = AstGenerator.gen(List(
      Connection.root("posts", fetch.posts),
      Connection.root("users", fetch.users),
      Connection.from("albums", fetch.userAlbums),
      Connection.from("comments", fetch.postComments),
      Connection.from("comments", fetch.userComments),
      Connection.from("photos", fetch.albumPhotos),
      Connection.from("posts", fetch.userPosts),
      Connection.from("user", fetch.albumUser),
      Connection.from("user", fetch.postUser),
      Connection.from("album", fetch.photoAlbum),
    ))

    val actual = AstPrinter.render(connection)

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
