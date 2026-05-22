# aeson-better-errors

A small package which gives you tools to build parsers to decode JSON values,
and gives good error messages when parsing fails.

* Announcement blog post / tutorial: <http://harry.garrood.me/blog/aeson-better-errors/>
* Hackage: <https://hackage.haskell.org/package/aeson-better-errors>

Note that since this package was released, aeson's default error output has
become much better. However, this library's error types provide more structure
than aeson's. If your use case benefits from structured errors, this library is
likely still a good choice, but otherwise, it's probably easier to use stock
aeson.
