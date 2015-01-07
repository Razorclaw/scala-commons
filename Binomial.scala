object Math {
    def binomial(n: Int, m: Int): Long = {
        val b = new Array[Long](n + 1)
        b(0) = 1
        for (i <- 1 to n) {
            b(i) = 1
            var j = i - 1
            while (j > 0) {
                b(j) = (b(j) + b(j - 1))
                j = j - 1
            }
        }
        b(m)
    }
}