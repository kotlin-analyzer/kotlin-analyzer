val x: Int
val (@Anno x: A.B, @Deco x: T.() -> Unit) = listOf(a, b)
val count by remember { mutableStateOf(0) }
val <T> List<T>.lastIndex: Int
   get() = this.size - 1
val greet: String.() -> Unit = { }
