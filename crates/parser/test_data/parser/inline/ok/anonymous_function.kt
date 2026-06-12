val a = fun(x: Int, y: Int): Int { return x + y }
val b = suspend fun(x: Int, y: Int): Int { return x + y }
val c = suspend context(a: A, b: B) fun(x: Int, y: Int): Int { return x + y }
val d = context(a: A, _: B) fun(x: Int, y: Int): Int { return x + y }
