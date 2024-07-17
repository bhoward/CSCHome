sealed trait Command
// Primitive commands:
case class Fwd(d: Double) extends Command
case class Back(d: Double) extends Command
case class Turn(a: Double) extends Command
case object Invisible extends Command
case object Visible extends Command
case class Speed(n: Int) extends Command
case class Pos(x: Double, y: Double) extends Command
case class Head(a: Double) extends Command
case class After(c1: Command, c2: Command) extends Command
case class Split(c1: Command, c2: Command) extends Command
case object Stop extends Command
case class Pen(c: Color) extends Command

// Derived commands:
val Left = Turn(90)
val Right = Turn(-90)
def Go(a: Double, d: Double): Command =
    After(Head(a), Fwd(d))

// Turn a command into a sequence of turtle actions
def perform(cmd: Command): Unit = cmd match {
    case Fwd(d) => forward(d)
    case Back(d) => back(d)
    case Turn(a) => turn(a)
    case Invisible => penUp
    case Visible => penDown
    case Speed(n) => setAnimationDelay(n)
    case Pos(x, y) => setPosition(x, y)
    case Head(a) => setHeading(a)
    case After(c1, c2) => perform(c1); perform(c2)
    case Split(c1, c2) => {
        // Save old state: color, position and heading
        val c = style.penColor
        val p = position
        val a = heading
        
        perform(c1)
        
        // Restore saved state
        setPenColor(c)
        setPosition(p)
        setHeading(a)
        
        perform(c2)
    }
    case Stop => {}
    case Pen(c) => setPenColor(c)
}

// Examples
val square = After(After(Go(0, 100), Go(90, 100)),
                   After(Go(180, 100), Go(270, 100)))

val plus = Split(Split(Go(0, 100), Go(90, 100)),
                 Split(Go(180, 100), Go(270, 100)))

clear
setAnimationDelay(100)
perform(Split(After(Go(60, 200), After(Pen(blue), plus)),
              After(Go(135, 300), After(Pen(green), plus))))


/*----------------------------------------------------------------*
 * Exercises, due Wednesday, September 14                         *
 *----------------------------------------------------------------*/

// 1. Write a function odometer(c: Command): Double that computes
// the total distance the turtle would travel (not counting jumps)
// while performing c.

// test("Distance of Stop") {
//   odometer(Stop) should equal(0)
// }
// 
// test("Distance of Plus") {
//   odometer(Plus) should equal(400)
// }

// 2. Write a function scale(c: Command, s: Double): Command that
// produces a new Command similar to c, except all of the distances
// have been scaled by a factor of s.

// test("Scale of Stop") {
//   scale(Stop, 1.5) should equal(Stop)
// }
// 
// val bigPlus = Split(Split(Go(0, 150), Go(90, 150)),
//                     Split(Go(180, 150), Go(270, 150)))
// test("Scale of Plus") {
//   scale(plus, 1.5) should equal(bigPlus)
// }

// 3. Declare a value house: Command that, when performed, draws a
// simple house starting at the current turtle position. Feel free to
// be creative; at a minimum, the house should have a rectangular front
// wall, a triangular roof, and at least one door or window.

// 4. Write a function street(n: Int): Command that produces a Command
// to draw a horizontal row of n houses. Be sure to use your house
// value from exercise 3; you may need to revise it to avoid absolute
// positions.

// 5. Write a function angledStreet(n: Int): Command that produces a
// Command to draw a diagonal row of n houses, where each is 10% smaller
// than the previous. Hint: use the scale function. Again, use your house
// value to draw each house.