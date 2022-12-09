package compose.graphql

import zio.parser.Syntax

package object ast {
  type GraphQLSyntax[A] = Syntax[String, Char, Char, A]
}
