object Test extends App {
  println(s"${foo:}")
  println(s"${foo: }")
  println(s"qwe${foo:}")
  println(s"qwe${foo:  }")
  println(s"${foo:}bar")
  println(s"${foo:  }bar")
  println(s"qwe${foo:}bar")
  println(s"qwe${foo:  }bar")
}