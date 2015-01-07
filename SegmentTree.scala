class SegmentTree[T: Manifest](a: IndexedSeq[T], f: (T, T) => T) {
    val t = new Array[T](4 * Integer.highestOneBit(a.length) + 1)
    
    build(1, 0, a.length - 1)
        
    private def build(n: Int, i: Int, j: Int) {
        if (i == j) {
            t(n) = a(i)
        } else {
            val k = (i + j) / 2
            val l = n * 2
            val r = l + 1
            build(l, i, k)
            build(r, k + 1, j)
            t(n) = f(t(l), t(r))
        }
    }
    
    private def q(n: Int, i: Int, j: Int, l: Int, r: Int) : T = {
        if (l == i && j == r) {
            t(n)
        } else {
            val k = (i + j) / 2
            if (l <= k) {
                if (r <= k) {
                    q(n * 2, i, k, l, r)
                } else {
                    val q1 = q(n * 2, i, k, l, k)
                    val q2 = q(n * 2 + 1, k + 1, j, k + 1, r)
                    f(q1, q2)
                }
            } else {
                q(n * 2 + 1, k + 1, j, l, r)
            }
        }
    }

    // TODO: integrate
    private def u(n: Int, a: Int, b: Int, i: Int, v: Int) : T = {
        if (a == b) {
            t(n) = t(n) * v
        } else {
            val c = (a + b) / 2
            val q = if (i <= c) u(n * 2, a, c, i, v) else u(n * 2 + 1, c + 1, b, i, v)
            val o = if (i <= c) t(n * 2 + 1) else t(n * 2)
            t(n) = f(q, o)
        }
        return t(n)
    }
    
    def query(l: Int, r: Int): T = q(1, 0, a.length - 1, l, r)
}