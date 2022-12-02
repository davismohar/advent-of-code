fun main(args: Array<String>) {
    println(P01.getSolution())
}

private fun Problem.getSolution() = format(problem, { part1(getInput(problem)) }, { part2(getInput(problem)) })
private fun format(puzzle: String, part1:  () -> Any, part2: () -> Any): String =
    """
    -----
    $puzzle
    part 1:
    ${part1()}
    part 2:
    ${part2()}
    ------
    """.trimIndent()
