import java.io.File
import java.util.Properties

fun Problem.getInput(problem: String) = File("src/main/resources/$problem/input.txt").readLines()
fun Problem.getTestInput() = File("src/test/resources/$problem/test.txt").readLines()
fun <T> List<T>.tail() = drop(1)
