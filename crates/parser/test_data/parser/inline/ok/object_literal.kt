val user = object {
    val name = "Alice"
    val age = 28
}
val customListener = object : ClickListener {
    override fun onClick() {
        println("Button clicked!")
    }
}
val dataObject = data object : DataInterface by DataImplementation(), DataMarker {
    val data = "Some data"
}
