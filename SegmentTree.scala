class BinaryTree[T: Manifest](a: IndexedSeq[T], f: (T, T) => T) {
    val t = new Array[T](4 * Integer.highestOneBit(a.length))
    
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
    
    def query(l: Int, r: Int): T = {
        def q(n: Int, i: Int, j: Int, l: Int, r: Int) : T = {
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
        q(1, 0, a.length - 1, l, r)
    }
    
    def update(idx: Int, val: T): Unit = {
        def u(n: Int, a: Int, b: Int, idx: Int, v: T) : T = {
            if (a == b) {
                t(n) = t(n) * v
            } else {
                val c = (a + b) / 2
                val q = if (idx <= c) u(n * 2, a, c, idx, v) else u(n * 2 + 1, c + 1, b, idx, v)
                val o = if (i <= c) t(n * 2 + 1) else t(n * 2)
                t(n) = f(q, o)
            }
            return t(n)
        }
    }
    

    
    override def toString = t.mkString(",")
        
    def display = {
        def d(n: Int, i: Int): Unit = if (n < t.length) {
            val indent = "" + ("   " * i)
            val sep = if (n % 2 == 0) "" else ""
            println(indent + sep + " " + t(n))
            d(n * 2, i + 1)
            d(n * 2 + 1, i + 1)
        }
        println(t(1))
        d(2, 1)
        d(3, 1)
    }

    // TODO: integrate update
    /*
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
    }*/
}

class Segment(val i: Long, val j: Long, val isLeaf: Boolean) {
    var k = 0
        
    override def toString = "[" + i + "," + j + "](" + k + ")"
}

class SegmentTree(val a: IndexedSeq[Segment], val f: (Segment, Segment) => Segment) extends BinaryTree[Segment](a, f) {
    val pts = new HashMap[Long, Long]
        
    def setEp(ep: IndexedSeq[Long]) {
        ep.foreach(p => pts(p) = 0)
    }
    
    private def addSegment(n: Int, s: Segment) {
        val cur = t(n)
        if (s.i == s.j) {
            pts(s.i) += 1
        } else if (s.i <= cur.i && s.j >= cur.j) {
            cur.k += 1
        } else if (!cur.isLeaf && ((s.i >= cur.i && s.i <= cur.j) || (s.j >= cur.i && s.j <= cur.j))) {
            addSegment(n * 2, s)
            addSegment(n * 2 + 1, s)
        }
    }
    
    def addSegments(segments: IndexedSeq[Segment]) =
        segments.foreach(s => addSegment(1, s))
        
    def query(r: Long) = q(1, r) + (if (pts contains r) pts(r) else 0)
        
    private def q(n: Int, r: Long): Int = {
        if (n >= t.length) return 0
        val cur = t(n)
        if (cur == null) return 0
        if (cur.i <= r && cur.j > r)
            cur.k + q(n * 2, r) + q(n * 2 + 1, r)
        else
            0
    }
}

