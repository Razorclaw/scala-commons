object Math {
    final def gcd(a: Int, b: Int): Int =
        if (b == 0) a else gcd(b, a % b)
        
    final def gcd(a: BigInt, b: BigInt): BigInt =
        if (b == 0) a else gcd(b, a % b)
}