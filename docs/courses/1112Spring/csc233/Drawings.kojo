sealed trait Drawing

trait Shape extends Drawing
case class Square(size: Double) extends Shape
case class Circle(size: Double) extends Shape
case class Polygon(points: List[(Double, Double)]) extends Shape

case object Nothing extends Drawing
case class Overlay(d1: Drawing, d2: Drawing) extends Drawing
case class Offset(d: Drawing, x: Double, y: Double) extends Drawing
case class Rotate(d: Drawing, a: Double) extends Drawing
case class Scale(d: Drawing, factor: Double) extends Drawing
case class PenColor(d: Drawing, c: Color) extends Drawing
case class FillColor(d: Drawing, c: Color) extends Drawing

// Turn a drawing (d) into commands for a turtle (t)
// using the given scale factor (s)
def draw(d: Drawing, t: Turtle = turtle0, s: Double = 1) {
    // Move the turtle x units right and y units forward, relative to pos,
    // _without_ drawing
    def jumpRelative(pos: Point, x: Double, y: Double) {
        val rad = math.Pi * t.heading / 180
        t.setPosition(pos.x + math.sin(rad) * x + math.cos(rad) * y,
                      pos.y - math.cos(rad) * x + math.sin(rad) * y)
    }
    
    // Move the turtle x units right and y units forward, relative to pos,
    // _while_ drawing
    def moveRelative(pos: Point, x: Double, y: Double) {
        val head = t.heading
        val rad = math.Pi * head / 180
        t.moveTo(pos.x + math.sin(rad) * x + math.cos(rad) * y,
                      pos.y - math.cos(rad) * x + math.sin(rad) * y)
        t.setHeading(head) // moveTo turned the turtle, so we have to turn back
    }
    
    // Draw a polygon relative to the given position, using the current scale (s)
    def drawPoly(pos: Point, points: List[(Double, Double)]): Unit = points match {
        case Nil => // Do nothing
            
        case (x, y) :: Nil =>
            jumpRelative(pos, s * x, s * y)
            
        case (x, y) :: rest =>
            drawPoly(pos, rest)
            moveRelative(pos, s * x, s * y)
    }
    
    // draw(d, t) should leave the state of t unchanged
    d match {
        case Square(size) =>
            repeat(4) {
                t.forward(s * size)
                t.right(90)
            }
            
        case Circle(size) =>
            val pos = t.position
            jumpRelative(pos, 0, s * size / 2)
            repeat(360) {
                t.forward(s * math.Pi * size / 360)
                t.right(1)
            }
            t.setPosition(pos.x, pos.y)
            
        case Polygon(points) =>
            val pos = t.position
            drawPoly(pos, points)
            t.setPosition(pos.x, pos.y)
            
        case Nothing => // do nothing
            
        case Overlay(d1, d2) =>
            draw(d1, t, s); draw(d2, t, s)
            
        case Offset(d, x, y) =>
            val pos = t.position
            jumpRelative(pos, s * x, s * y)
            draw(d, t, s)
            t.setPosition(pos.x, pos.y)
            
        case Rotate(d, a) =>
            t.left(a)
            draw(d, t, s)
            t.right(a)
            
        case Scale(d, factor) =>
            draw(d, t, s * factor)
            
        case PenColor(d, c) =>
            t.saveStyle() // pushes pen color, etc. on stack
            t.setPenColor(c)
            draw(d, t, s)
            t.restoreStyle() // pops saved style off stack

        case FillColor(d, c) =>
            t.saveStyle() // pushes pen color, etc. on stack
            t.setFillColor(c)
            draw(d, t, s)
            t.restoreStyle() // pops saved style off stack
    }
}

// functions on drawings:

// Returns (lower left x, lower left y, width, height) of d
def size(d: Drawing): (Double, Double, Double, Double) = d match {
    case Square(size) =>
        (0, 0, size, size)
        
    case Circle(size) =>
        (0, 0, size, size)
        
    case Polygon(points) =>
        def bounds(ps: List[(Double, Double)]): (Double, Double, Double, Double) = ps match {
            case Nil => (0, 0, 0, 0)
            case (x, y) :: Nil => (x, y, x, y)
            case (x, y) :: rest =>
                val (llx, lly, urx, ury) = bounds(rest)
                (x min llx, y min lly, x max urx, y max ury)
        }
        
        val (llx, lly, urx, ury) = bounds(points)
        (llx, lly, urx - llx, ury - lly)
        
    case Nothing =>
        (0, 0, 0, 0)
        
    case Overlay(d1, d2) =>
        val (llx1, lly1, w1, h1) = size(d1)
        val (llx2, lly2, w2, h2) = size(d2)
        val llx = llx1 min llx2
        val lly = lly1 min lly2
        val urx = (llx1 + w1) max (llx2 + w2)
        val ury = (lly1 + h1) max (lly2 + h2)
        (llx, lly, urx - llx, ury - lly)
        
    case Offset(d1, x, y) =>
        val (llx1, lly1, w1, h1) = size(d1)
        (x + llx1, y + lly1, w1, h1)
        
    case Rotate(d1, a) =>
        val (llx1, lly1, w1, h1) = size(d1)
        val (urx1, ury1) = (llx1 + w1, lly1 + h1)
        val rad = math.Pi * a / 180
        
        def rotx(x: Double, y: Double): Double = x * math.cos(rad) - y * math.sin(rad)
        def roty(x: Double, y: Double): Double = x * math.sin(rad) + y * math.cos(rad)
        
        val llx = rotx(llx1, lly1) min rotx(llx1, ury1) min rotx(urx1, ury1) min rotx(urx1, lly1)
        val urx = rotx(llx1, lly1) max rotx(llx1, ury1) max rotx(urx1, ury1) max rotx(urx1, lly1)
        val lly = roty(llx1, lly1) min roty(llx1, ury1) min roty(urx1, ury1) min roty(urx1, lly1)
        val ury = roty(llx1, lly1) max roty(llx1, ury1) max roty(urx1, ury1) max roty(urx1, lly1)
        (llx, lly, urx - llx, ury - lly)
        
    case Scale(d1, f) =>
        val (llx1, lly1, w1, h1) = size(d1)
        (f * llx1, f * lly1, f * w1, f* h1)
        
    case PenColor(d1, c) =>
        size(d1)
        
    case FillColor(d1, c) =>
        size(d1)
}


// note that this is not a new primitive shape
def Rectangle(width: Double, height: Double): Drawing =
    Polygon(List((0, 0), (width, 0), (width, height), (0, height), (0, 0)))

def frame(d: Drawing): Drawing = {
    val (x, y, w, h) = size(d)
    Overlay(d,
            Offset(Rectangle(w, h), x, y))
}


def parade(d: Drawing, n: Int, x: Double, y: Double): Drawing = n match {
    case 0 => Nothing
    case _ => Overlay(d, Offset(parade(d, n-1, x, y), x, y))
}

// Alternate, more general approach:
def parade2(d: Drawing, n: Int, x: Double, y: Double): Drawing = {
    // Auxilliary function to produce i copies of d in a parade
    def aux(i: Int): Drawing = i match {
        case 0 => Nothing
        case _ => Overlay(d, Offset(aux(i-1), x, y))
    }
    
    aux(n)
}

def beside(d1: Drawing, d2: Drawing): Drawing = {
    val (x, y, w, h) = size(d1)
    Overlay(d1, Offset(d2, x+w, 0))
}


// test
clear()
setAnimationDelay(0)
invisible()


val d = Overlay(Overlay(Square(100), Circle(100)),
                FillColor(Polygon(List((50, 20), (80, 50), (20, 50), (50, 20))), yellow))
val d2 = Overlay(Overlay(Square(100), PenColor(Rotate(Scale(d, 0.7), -45), blue)),
             Offset(FillColor(Scale(d, 2), green), 100, 50))
// draw(Rectangle(300, 20))
// draw(frame(d2))
// draw(parade2(d, 5, 150, -20))
draw(frame(beside(d2, d)))

//********************* Exercises **************************
//
// Uncomment each exercise, finish the code, and check that it
// passes any tests. Write additional tests as you see fit.

/*
// 1. Write a function with signature
//     IsoTriangle(width: Double, height: Double): Drawing
//   that produces an isosceles triangle. The base should have
//   the given width, while the two equal sides should rise to
//   meet in the middle at the given height.


test("Size of IsoTriangle") {
    size(IsoTriangle(100, 50)) should equal((0, 0, 100, 50))
}
*/

/*
// 2. Write a function with signature
//     star(d: Drawing, n: Int): Drawing
//   that produces a Drawing consisting of n copies of d, rotated
//   equally around the origin. For example,
//   star(Polygon(List((0, 0), (0, 100))), 6)
//   should draw six lines radiating out, like an asterisk.


test("Size of square star") {
    size(star(Square(100), 4)) should equal((-100, -100, 200, 200))
}
*/

/*
// 3. Write a function with signature
//     above(d1: Drawing, d2: Drawing): Drawing
//   that produces a Drawing with d2 arranged so that its bounding
//   box is directly above (in the positive y direction) that of d1.


test("Size of circle above square") {
    size(above(Square(100), Circle(200))) should equal((0, 0, 200, 300))
}
*/

/*
// 4. Write a function with signature
//     distance(d: Drawing): Double
//   that returns the total distance travelled by the turtle when
//   drawing d. Only count when the pen is down, not any jumps.

 
test("Distance of a drawing") {
    distance(Square(100)) should equal(400.0)
    distance(Polygon(List((0, 0), (40, 0), (0, 30), (0, 0)))) should
        equal(120.0)
}
*/

/*
// 5. Use the turtle drawing language to create an interesting picture.
//   It should include several primitive shapes and several of the
//   ways of combining drawings, and it should be constructed in pieces
//   by calling suitable functions, either from the above examples or
//   of your own devising.
 */