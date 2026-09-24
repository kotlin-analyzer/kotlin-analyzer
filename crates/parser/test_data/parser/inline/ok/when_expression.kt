val a = when (x) {
 1 -> println("one")
 2 -> println("two")
 else -> println("other")
}
val b = when (currentAge) {
    in 0..12 -> "Child"
    in 13..19 -> "Teenager"
    in 20..64 -> "Adult"
    else -> "Senior"
}
fun processData(obj: Any): String = when (obj) {
    is String -> "String of length ${obj.length}"
    is Int if obj > 0 -> "Integer multiplied: ${obj * 2}"
    is Int -> "Integer addition: ${obj + 2}"
    else -> "Unknown type"
}
val c = when (val input = "yes") {
    "yes" -> "You said yes"
    "no" -> "You said no"
    else -> "Unrecognized input: $input"
}
