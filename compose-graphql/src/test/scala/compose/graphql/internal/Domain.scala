package compose.graphql.internal

import compose.macros.DeriveAccessors
import zio.schema.{DeriveSchema, Schema}

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
