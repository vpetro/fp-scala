package com.some.code

object Hello extends App {
  val p = Person("Big Rich")
  println("Hello from " + p.name)
}

//Person class - concise Scala defintion - nice!
case class Person(var name: String)

