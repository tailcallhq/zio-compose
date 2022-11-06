package compose

import zio.parser.Syntax

package object graphql {
  type GraphQLSyntax[A] = Syntax[String, Char, Char, A]
}
