object P01 : Problem {
    override val problem = "p01"

    override val part1: (List<String>) -> Int = { input ->
        splitAtEmptyStrings(input, emptyList(), CalorieList(emptyList()))
            .map { it.toElf() }
            .maxOf { it.caloriesHeld }
    }
    override val part2: (List<String>) -> Int = { input ->
        splitAtEmptyStrings(input, emptyList(), CalorieList())
            .map { it.toElf() }
            .sortedBy { it.caloriesHeld }
            .reversed()
            .take(3)
            .sumOf { it.caloriesHeld }
    }

    private data class CalorieList(val calories: List<Int> = emptyList()) {
        fun toElf() = Elf(calories.sum())
    }

    private data class Elf(val caloriesHeld: Int)

    private fun splitAtEmptyStrings(
        strings: List<String>,
        completed: List<CalorieList> = emptyList(),
        curr: CalorieList = CalorieList(),
    ): List<CalorieList> =
        when {
            strings.isEmpty() -> completed + curr
            strings.first().isBlank() -> splitAtEmptyStrings(strings.tail(), completed + curr, CalorieList(emptyList()))
            else -> splitAtEmptyStrings(
                strings.tail(),
                completed,
                curr.copy(calories = curr.calories + strings.first().toInt())
            )
        }
}
