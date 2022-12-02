import kotlin.test.assertEquals
import kotlin.test.Test

class P01Test {
    @Test
    fun `p01 part 1`() {
        assertEquals(24000, P01.part1(P01.getTestInput()))
    }

    @Test
    fun `p01 part 2`() {
        assertEquals(45000, P01.part2(P01.getTestInput()))
    }
}
