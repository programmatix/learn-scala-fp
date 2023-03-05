import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.io.StdIn.readLine

object Main extends App {

  // Implementing map for List[A]
  def map[A, B](in: List[A], mapper: A => B): List[B] = in match {
    case x :: Nil => List(mapper(x))
    case x :: xs  => mapper(x) +: map(xs, mapper)
  }

  // tailrec version
  @tailrec
  def map[A, B](in: List[A], acc: List[B], mapper: A => B): List[B] = in match {
    case x :: Nil => mapper(x) +: acc
    case x :: xs  => map(xs, mapper(x) +: acc, mapper)
  }

  // Imperative version
  def mapImperative[A, B](in: List[A], mapper: A => B): List[B] = {
    for {
      x <- in
    } yield mapper(x)
  }

  // Implementing filter for List[A]
  def filter[A](in: List[A], f: A => Boolean): List[A] = in match {
    case x :: Nil => if (f(x)) List(x) else List()
    case x :: xs  => if (f(x)) x +: filter(xs, f) else filter(xs, f)
  }

  // Call-by-name.
  // Basically toProfile is going to be evaluated on use.
  // The cool thing here is that we only need to care about toProfile's return type.  The rest
  // of its type can be arbritarily complex.  E.g. this lets us generally test any block of code.
  def profile[A](toProfile: => A): (A, Long) = {
    val start     = System.nanoTime()
    val result: A = toProfile
    val end       = System.nanoTime()
    (result, start - end)
  }

  profile(System.nanoTime)
  profile(map(List(1, 2, 3), (x: Int) => x * 2))

  // Another call-by-name example to add an assert.
  def myAssert(check: => Boolean) = {
    if (!check) {
      throw new AssertionError()
    }
  }

  myAssert(5 > 3)
  // Without call-by-name, would have to write
  // myAssert(() => 5 > 3)

  // Using multiple parameter groups to e.g. implement my own while loop.
  @tailrec
  def whilst(guard: => Boolean)(body: => Unit): Unit = {
    if (guard) {
      body
      whilst(guard)(body)
    }
  }

  var i = 0
  whilst(i < 5) {
    println("Hello")
    i += 1
  }

  // Implicits.
  def needsImplicit(normalArg: Int)(implicit implictArg: Int): Int =
    normalArg * implictArg

  needsImplicit(1)(2)
  implicit val someInt = 42
  needsImplicit(1)

  // Currying.
  // A function that takes multiple args, can be translated into a series of function calls
  // that each take a single arg.
  // Currying is heavily related to the idea of partially applying a function (which is a more
  // practical and useful thing to understand).
  // Partial application is applying a function to just some of its args.
  def curryMe(x: Int)(y: Int) = x + y

  def partiallyApplied = curryMe(1)(_)

  def finalResult = partiallyApplied(2)

  // Alternative syntax:
  def alsoPartiallyApplied = curryMe(1) _

  // A useful example of currying:
  def wrap(prefix: String)(body: String)(suffix: String) = prefix + body + suffix

  val someHtml = wrap("<div>")("hello world")("/div")

  // Currying lets us do that more generically:
  val divMe = wrap("<div>")(_: String)("/div")
  val pMe   = wrap("<p>")(_: String)("/p")

  val someHtml2 = divMe("hello world")

  // Can also do partially applied functions without multiple parameter groups:
  def wrapTwo(prefix: String, body: String, suffix: String) =
    prefix + body + suffix

  val wrapWithDiv = wrapTwo("<div>", _: String, "</div>")
  wrapWithDiv("hello world")

  // Advanced stuff:  .curried lets us curry a method.
  def add(x: Int, y: Int) = x + y
  // val addCurried = add.curried
  // addCurried(1)(2)

  // Except we can't do that, we have to do Eta expansion first to turn the add method into an
  // add function.
  val addAsFunction = add _
  val addCurried    = addAsFunction.curried
  addCurried(1)(2)

  // This has basically produced
  // def addCurried(x: Int)(y: Int) = x + y


  // Partial functions (not in the book) are different.
  // They represent applying some only cases in a pattern match.

  // Complete pattern match:
  val donut = "Glazed Donut"
  val tasteLevel = donut match {
    case "Glazed Donut" | "Strawberry Donut" => "Very tasty"
    case "Plain Donut" => "Tasty"
    case _ => "Tasty"
  }

  // Part of the match, as a PartialFunction:
  val isVeryTasty: PartialFunction[String, String] = {
    case "Glazed Donut" | "Strawberry Donut" => "Very Tasty"
  }

  // Will get MatchError if this doesn't match
  val partialFuncResult: String = isVeryTasty(donut)

  val unknownTaste: PartialFunction[String, String] = {
    case donut: _ => s"Unknown taste for donut = $donut"
  }

  val willMatchAll: PartialFunction[String, String] = isVeryTasty orElse unknownTaste

  val partialFuncResult2: String = willMatchAll(donut)


  // .collect() - also not in book, but very useful
  // Takes a PartialFunction.
  // Acts as both filter and map.  Only the results the PartialFunction matches will be returned.


  // Making lists
  List() == Nil
  1 :: 2 :: 3 :: Nil

  // Practicing recursion
  def reverseList[A](in: List[A]): List[A] = in match {
    case Nil     => Nil
    case x :: xs => reverseList(xs) :+ x
  }

  def reverseString(in: String): String = in.toList match {
    case x if x.isEmpty => new String()
    case x :: xs        => reverseString(xs.toString) + x
  }

  // Simple recursive sum
  // Note it's not tail recursive despite sum() being the last thing in there.
  // This is because really that last line is `val s = sum(tail); val result = head + s; return result;`
  def sum(xs: List[Int]): Int = xs match {
    case Nil          => 0
    case head :: tail => head + sum(tail)
  }

  // Can make tail recursive by changing to an accumulator
  @tailrec
  def sum(acc: Int, xs: List[Int]): Int = xs match {
    case Nil          => 0
    case head :: tail => sum(head + acc, tail)
  }

  // This is usually written in this style:
  def sumTailRec(xs: List[Int]): Int = {
    @tailrec
    def sumInternal(xs: List[Int], acc: Int): Int = xs match {
      case Nil          => 0
      case head :: tail => sumInternal(tail, head + acc)
    }

    sumInternal(xs, 0)
  }

  def fibonacciList(upTo: Int): List[Int] = {
    @tailrec
    def fibInternal(upTo: Int, acc: List[Int]): List[Int] = {
      if (upTo == 0) {
        acc
      } else {
        acc match {
          case x :: y :: xs => fibInternal(upTo - 1, (x + y) :: acc)
        }
      }
    }

    fibInternal(upTo, 1 :: 0 :: Nil)
  }

  def fibonacciNum(upTo: Int): Int = {
    @tailrec
    def fibInternal(upTo: Int, cur: Int, prev: Int): Int = {
      if (upTo == 0) {
        cur
      } else {
        fibInternal(upTo - 1, cur + prev, cur)
      }
    }

    fibInternal(upTo, 1, 0)
  }

  def coinTossGame(): Unit = {
    case class GameState(numFlips: Int, correctFlips: Int)

    sealed trait GameAction
    object GameAction {
      case object Quit extends GameAction

      case object Continue extends GameAction

      case object NewGame extends GameAction
    }

    @tailrec
    def main(cur: GameState, pastGames: List[GameState]): Unit = {
      val flip = ThreadLocalRandom.current().nextInt(2) match {
        case 0 => "H"
        case 1 => "T"
      }

      val input = readLine("Input state 'h' (heads), 't' (tails) or 'q' (quit)").toUpperCase
      val (nextState, nextAction) = input match {
        case "H" | "T" =>
          val matched = flip == input
          (
            cur.copy(
              numFlips = cur.numFlips + 1,
              correctFlips = cur.correctFlips + (if (matched) 1 else 0)
            ),
            GameAction.Continue
          )
        case "Q" =>
          println(cur)
          (cur, GameAction.Quit)
        case "N" =>
          (GameState(0, 0), GameAction.NewGame)
        case _ =>
          println(s"Did not understand input ${input}")
          (cur, GameAction.Continue)
      }
      nextAction match {
        case GameAction.Quit     =>
        case GameAction.Continue => main(nextState, pastGames)
        case GameAction.NewGame  => main(nextState, pastGames :+ cur)
      }
    }

    main(GameState(0, 0), Nil)
  }

  // coinTossGame()

  // Codility challenge MissingInteger - find minimum positive number >0 not in the input
  object Solution {
    def solution(a: List[Int]): Int = {
      @tailrec
      def internal(remaining: List[Int], acc: Set[Int]): Int = remaining match {
        case Nil     => acc.min
        case x :: xs => internal(xs, acc - x)
      }

      internal(a.toList, Range(1, 100_000).toSet)
    }

    assert(5 == Solution.solution(List(1, 3, 6, 4, 1, 2)))
    assert(4 == Solution.solution(List(1, 2, 3)))
    assert(1 == Solution.solution(List(-1, -3)))
  }

  // For-comprehensions.
  // Get converted by compiler into series of map, flatMap, foreach, withFilter.  It's just sugar.
  case class Person(firstName: String, lastName: String)

  val people = List(
    Person("barney", "rubble"),
    Person("fred", "flintstone")
  )

  val namesStartingWithB: List[String] = for {
    p <- people // generator
    fname = p.firstName // definition
    if (fname startsWith "b") // filter
  } yield fname

  // What data types can be used in for-comprehensions?
  // If it has .map(), can be used in for comprehensions containing a single generator.
  // If it has .flatMap() as well, can be used in ones containing multiple generators.
  // If it has .withFilter(), can be used for filters.
  // If it has .foreach(), can be used in a regular for loop e.g. `for (i <- thingies)`
  // This class could do the lot:

  abstract class CustomClass[A] {
    def map[B](f: A => B): CustomClass[B]

    def flatMap[B](f: A => CustomClass[B]): CustomClass[B]

    def withFilter(p: A => Boolean): CustomClass[A]

    def foreach(b: A => Unit): Unit
  }

  case class Sequence[A](initialElems: A*) {
    private val elems = scala.collection.mutable.ArrayBuffer[A]()
    elems ++= initialElems

    def foreach(f: A => Unit): Unit = elems.foreach(f)
    //    def map[B](f: A => B): Sequence[B] = Sequence(elems.map(f))
    //    def withFilter(f: A => Boolean): Sequence[A] = Sequence(elems.withFilter(f))
    //    def flatMap[B](f: A => Sequence[B]): Sequence[B] = {
    //      val mapped: Sequence[Sequence[B]] = map(f)
    //      // flatten not written but if it were would now do:
    //      // flatten(mapped)
    //      ???
    //    }
  }

  // Understanding flatMap.
  // On sequences, works much like map + flatten.  (Flatten takes a list of lists, and produces a flattened list of elements.)

  // Either.
  // Used to have no preference between Left and Right, it was just convention to have Left=error, Right=success.
  // As of 2.12 they fixed that.  It's now Right/success biased.  Can now use in a for-comprehension or map or flatMap,
  // and it'll assume you're working with the success case.

  // He prefers Option and Try for error handling, over Either.  Of course, don't throw!

  // Advanced blocks:
  def takesInt(input: => Int): Unit = println("Got int")

  def takesFunction(input: Int => Int): Unit = println("Got func")

  takesInt {
    42
  }

  takesFunction { a =>
    a * 2
  }

  // Bind.
  def f(a: Int): (Int, String) = ???

  def g(a: Int): (Int, String) = ???

  def h(a: Int): (Int, String) = ???

  def bind(f: Int => (Int, String), result: (Int, String)): (Int, String) = {
    val next = f(result._1)
    (next._1, next._2 + result._2)
  }

  def bindTest() {
    val fResult = f(100)
    val gResult = bind(g, fResult)
    val hResult = bind(h, gResult)
  }

  case class Wrapper[T](value: T) {
    def map[Y](f: T => Y): Wrapper[Y] = Wrapper(f(value))

    def flatMap[Y](f: T => Wrapper[Y]): Wrapper[Y] = f(value)
  }

  val result: Wrapper[Int] = for {
    a <- new Wrapper(1)
    b <- new Wrapper(2)
    c <- new Wrapper(3)
  } yield a + b + c

  case class Debuggable[T](value: T, message: String) {
    def map[Y](f: T => Y): Debuggable[Y] = Debuggable(f(value), message)

    def flatMap[Y](f: T => Debuggable[Y]): Debuggable[Y] = {
      val next = f(value)
      Debuggable(next.value, next.message + " " + message)
    }
  }

  // State monads.
  case class GolfState(distance: Int)

  def swing(distance: Int, dbg: String): State[GolfState, Int] = State { (s: GolfState) =>
    val newAmount = s.distance + distance
    println(s"${dbg}: dist=${distance} new=${newAmount}")
    (GolfState(newAmount), newAmount)
  }

  println("State buildtime")

  // The point of a State monad is to allow writing code like this:
  val stateWithNewDistance: State[GolfState, Int] = for {
    _             <- swing(20, "A")
    _             <- swing(15, "B")
    totalDistance <- swing(0, "C")
  } yield totalDistance

  val beginningState = GolfState(0)
  println("State runtime")
  val finalState: (GolfState, Int) = stateWithNewDistance.run(beginningState) // will be 35

  case class State[S, A](run: S => (S, A)) {
    def flatMap[B](g: A => State[S, B]): State[S, B] = State { (s0: S) =>
      println("flatMap")
      val (s1, a) = run(s0)
      val next = g(a)
      next.run(s1)
    }

    def map[B](f: A => B): State[S, B] = flatMap(a => State.point(f(a)))
  }

  object State {
    def point[S, A](v: A): State[S, A] = State(run = s => (s, v))
  }

  // This is complex, trying to walk it through:
  // GolfState(0) simple enough.
  // stateWithNewDistance returns a State.
  // A State is a case class with single field - a run function.
  // I guess calling run() evaluates the full for-comprehension.  But how.
  // Each run() is going to ultimately call our swing method, which calculates our new state.
  // So it's a bit like reactive, with build time vs execution time.
  // The for-comprehension is building 3 States via State.flatMap, which (maybe) recursively know about each other.
  // Executing the States calls the run() functions, which point at swing().
  //
  // So during the for-comprehension (build time):
  // * swing(20) is called.  Immediately returns a State with a run() that will later:
  //   Take a state s.  Add distance 20 to s.distance, and spit out a GolfState containing that new distance.
  // * Now swing(15) is called.  Does same.
  // * And same again for swing(0).
  // Ok so conceptually alright so far: each State is going to, at "runtime", take a previous GolfState, apply a change
  // to it, return an update GolfState.
  //
  // So mechanically how do things actually work at runtime:
  // * Call stateWithNewDistance.run(beginningState)
  // * Somehow we're in State.flatMap.  s0 is beginningState - GolfState(0).
  // * So run is pointing at flatMap?  Unclear.
  // * Call run(s0)
  // * This executes swing(20).
  // * Ok so we must be executing the State that swing(20) returned.
  // * Returns a GolfState(20) to flatMap.
  // * flatMap has been passed a method g, that's going to turn a GolfState into a State.
  // * We call next = g(20).
  // * This goes back into the for-comprehension.  Unclear.
  // * next.run(GolfState(20)).
  // * Back into State.flatMap.
  // * Ok, don't totally get what's happening mechanically, but get the broad strokes.
  // * Can see that both flatMap and swing are only called at runtime.
  //
  // This could help: https://www.reddit.com/r/scala/comments/ehkzrq/state_monad_learn_how_it_works/
  //
  // Something the book doesn't do a good job on - _why_ I'd want to write code like this, instead of a simpler
  // procedural style.  It's deferring all state manipulation until runtime (impure core) - but at quite a cost of
  // complexity.


  // Mixing Monads in a for-comprehension is annoyingly impossible.  Can't have Trys mixed with Options.
  // Because under the hood it's a bunch of flatMap calls, so the types have to match.
  // Solutions include Free monads, monad zippers and views, monad coproducts, monad transformers.

  // Monad transformers.
  // Generally end in T - OptionT,EitherT,StateT (all in Cats).

  // First: lift/point/pure (different names, same concept).
  // "Lifting" a simple value into a Monad.  `Some(1)` is lifting 1 into an Option.
  //
  // Ok a huge section now that I'll have to come back to, about Monad transformers.  Results in code that allows
  // IO input and passes state around this big for expression that recursively calls itself, and is all effect driven
  // so it only evaluates at runtime:

//  def sumLoop: StateT[IO, SumState, Unit] = for {
//    _ <- putStrAsStateT("\ngive me an int, or 'q' to quit: ")
//    input <- getLineAsStateT
//    _ <- if (input == "q") {
//      liftIoIntoStateT(IO(Unit))
//    } else for {
//      i <- liftIoIntoStateT(IO(toInt(input)))
//      _ <- doSumWithStateT(i)
//      _ <- sumLoop
//    } yield Unit
//  } yield Unit
//  val result = sumLoop.run(SumState(0)).run
//  println(s"Final SumState: ${result}")


  // Cats


}
