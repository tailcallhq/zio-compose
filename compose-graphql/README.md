## GraphQL Compose

Built on top of [ZIO Compose]. GraphQL compose intends to solve the problem of API Composition.
It comes in two parts —

1. **DSL:** This is used to write the composition spec. Once the spec is written, it's validated and optimized by the compiler.
2. **Runtime:** A high-performance runtime that executes takes in a graphQL request and resolves it using a composition spec.

## Demo

A demo usage for [jsonplaceholder] is available here —

1. [DSL Usage]
2. [Unit Tests]

[jsonplaceholder]: https://jsonplaceholder.typicode.com/
[Unit Tests]: https://github.com/tailcallhq/zio-compose/blob/main/compose-graphql/src/test/scala/compose/graphql/JsonPlaceholderSpec.scala
[DSL Usage]: https://github.com/tailcallhq/zio-compose/blob/main/compose-graphql/src/test/scala/compose/graphql/internal/JsonPlaceholder.scala

[ZIO Compose]: https://github.com/tailcallhq/zio-compose