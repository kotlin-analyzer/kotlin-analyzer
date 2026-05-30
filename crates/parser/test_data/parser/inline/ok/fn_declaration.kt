fun foo() {}
fun foo(bar: Int) {}
fun foo(bar: Int = 2, baz: Int, meh: String = "") {}
fun <T> foo(@Anno bar: T) {}
fun <T> foo(): Int {}
fun <T> Receiver.foo(): Int {}
fun <T> T.foo(bar: Int, baz: T): Int where T: Any, T: Serializable {}
infix fun Int.shl(x: Int): Int
fun <T> asList(vararg ts: T): List<T>
fun double(x: Int): Int = x * 2
