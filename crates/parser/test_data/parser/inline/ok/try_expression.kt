val a = try { println("try block") } catch (e: Exception) {} finally {}
val b = try { println("try block") } catch (e: Exception) {} catch (t: Throwable) {}
val c = try { println("try block") } finally {}
