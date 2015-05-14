object Math2 {
    def ipow(b: Int, e: Int): Int = {
        var base = b
        var exp = e
        var result = 1;
        while (exp != 0) {
            if ((exp & 1) == 1)
                result *= base;
            exp = exp >> 1;
            base = base * base;
        }
        result;
    }
}