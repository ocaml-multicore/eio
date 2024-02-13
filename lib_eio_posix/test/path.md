```ocaml
# #require "eio_posix"
```
```ocaml
module P = Eio_posix__Path

let dump f p =
  Fmt.pf f "%a (%S)" P.dump p (P.to_string p)
```

```ocaml
# #install_printer dump;;

# P.parse "foo"
- : P.t = "foo" ("foo")

# P.parse "foo/bar"
- : P.t = "foo" / "bar" ("foo/bar")

# P.parse "foo//bar/"
- : P.t = "foo" / "bar" / ("foo/bar/")

# P.parse "foo/."
- : P.t = "foo" / ("foo/")

# P.parse "foo/./"
- : P.t = "foo" / ("foo/")

# P.parse ""
- : P.t = . (".")

# P.parse "."
- : P.t = . (".")

# P.parse ".."
- : P.t = .. / . ("..")

# P.parse "./../.././.."
- : P.t = .. / .. / .. / . ("../../..")

# P.parse "/"
- : P.t = / . ("/")

# P.parse "/etc"
- : P.t = / "etc" ("/etc")

# P.parse "/etc/passwd"
- : P.t = / "etc" / "passwd" ("/etc/passwd")

# P.parse "/."
- : P.t = / . ("/")

# P.parse "/.."
- : P.t = / .. / . ("/..")

# P.parse "//../"
- : P.t = / .. / . ("/..")
```
