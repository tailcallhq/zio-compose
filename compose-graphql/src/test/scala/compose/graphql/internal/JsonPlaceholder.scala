package compose.graphql.internal

import compose.graphql.Graph
import compose.macros.DeriveAccessors
import compose.model.http.Request
import compose.{Lambda, ~>}
import zio.schema.{DeriveSchema, Schema}

object JsonPlaceholder {
  final case class Post(userId: Int, id: Int, title: String, body: String)

  object Post {
    implicit val schema: Schema[Post] = DeriveSchema.gen[Post]
    val lens                          = DeriveAccessors.gen[Post]
  }

  final case class User(
    id: Int,
    name: String,
    username: String,
    email: String,
    address: Address,
    phone: String,
    website: String,
    company: Company,
  )

  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]
    val lens                          = DeriveAccessors.gen[User]
  }

  final case class Address(street: String, suite: String, city: String, zipcode: String, geo: Geo)

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
    val lens                             = DeriveAccessors.gen[Address]
  }

  final case class Geo(lat: String, lng: String)

  object Geo {
    implicit val schema: Schema[Geo] = DeriveSchema.gen[Geo]
    val lens                         = DeriveAccessors.gen[Geo]
  }

  final case class Company(name: String, catchPhrase: String, bs: String)

  object Company {
    implicit val schema: Schema[Company] = DeriveSchema.gen[Company]
    val lens                             = DeriveAccessors.gen[Company]
  }

  final case class Album(userId: Int, id: Int, title: String)

  object Album {
    implicit val schema: Schema[Album] = DeriveSchema.gen[Album]
    val lens                           = DeriveAccessors.gen[Album]
  }

  final case class Photo(albumId: Int, id: Int, title: String, url: String, thumbnailUrl: String)

  object Photo {
    implicit val schema: Schema[Photo] = DeriveSchema.gen[Photo]
    val lens                           = DeriveAccessors.gen[Photo]
  }

  final case class Comment(postId: Int, id: Int, name: String, email: String, body: String)

  object Comment {
    implicit val schema: Schema[Comment] = DeriveSchema.gen[Comment]
    val lens                             = DeriveAccessors.gen[Comment]
  }

  object fetch {
    import Lambda._
    def users: Any ~> List[User]        = Lambda
      .constant(Request(url = "https://jsonplaceholder.typicode.com/users")) >>>
      Lambda.http.decode[List[User]]
        .fold(Lambda.constant(List.empty[User]))(Lambda.identity[List[User]])
    def posts: Any ~> List[Post]        = Lambda
      .constant(Request(url = "https://jsonplaceholder.typicode.com/posts")) >>>
      Lambda.http.decode[List[Post]]
        .fold(Lambda.constant(List.empty[Post]))(Lambda.identity[List[Post]])
    def postUser: Post ~> User          = Lambda.die
    def userPosts: User ~> List[Post]   = Lambda.die
    def userAlbums: User ~> List[Album] = Lambda.die

    def albums: UserId ~> List[Album] = Lambda
      .constant(Request(url = "https://jsonplaceholder.typicode.com/albums")) >>>
      Lambda.http.decode[List[Album]]
        .fold(Lambda.constant(List.empty[Album]))(Lambda.identity[List[Album]])

    def albumPhotos: Album ~> List[Photo]   = Lambda.die
    def postComments: Post ~> List[Comment] = Lambda.die
    def albumUser: Album ~> User            = Lambda.die
    def userComments: User ~> List[Comment] = Lambda.die
    def photoAlbum: Photo ~> Album          = Lambda.die
  }

  case class UserId(userId: Option[Int])
  object UserId {
    implicit val schema: Schema[UserId] = DeriveSchema.gen[UserId]
    val lens                            = DeriveAccessors.gen[UserId]
  }

  val graph: Graph = Graph[Unit, Unit]("posts", fetch.posts) ++
    Graph[Unit, Unit]("users", fetch.users) ++
    Graph[UserId, Unit]("albums", fetch.albums <<< Lambda._1) ++
    Graph[Unit, User]("albums", fetch.userAlbums <<< Lambda._2) ++
    Graph[Unit, Post]("comments", fetch.postComments <<< Lambda._2) ++
    Graph[Unit, User]("comments", fetch.userComments <<< Lambda._2) ++
    Graph[Unit, Album]("photos", fetch.albumPhotos <<< Lambda._2) ++
    Graph[Unit, User]("posts", fetch.userPosts <<< Lambda._2) ++
    Graph[Unit, Album]("user", fetch.albumUser <<< Lambda._2) ++
    Graph[Unit, Post]("user", fetch.postUser <<< Lambda._2) ++
    Graph[Unit, Photo]("album", fetch.photoAlbum <<< Lambda._2)
}
