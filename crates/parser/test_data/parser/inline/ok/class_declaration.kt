class Foo1
class Foo2()
class Foo22(name: String, age: Int)
class Foo23<T>(name: T, val age: Int)
class Foo3<T> : Bar by baz
class Foo4<T> where T: Any
class Foo5<T> where T: Any, T: Serializable
class Foo7<T> where T: Any, T: Serializable {}
class Foo8 private constructor(val name: String, var age: Int)
fun interface Foo9<T> where T: Any, T: Serializable
enum class Foo10<T> where T: Any, T: Serializable {}
abstract class Foo11<T>(val name: String, val age: Int) where T: Any, T: Serializable
data class Foo12<T>(val name: String, val age: Int)
class A
{}
