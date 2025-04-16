package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, private var _battery: Int = 100) extends Robot:
  export robot.{position, direction, turn}
  def battery: Int = _battery
  private def batteryDec(): Unit = _battery -= 20

  override def act(): Unit =
    if battery >= 20 then
      robot.act()
      batteryDec()
      println(s"${robot.toString} - battery: $battery%")
    else
      println(s"Not enough battery ($battery%) to perform the action")
      throw IllegalStateException("Not enough battery ($battery%) to perform the action")

class RobotCanFail(val robot: Robot, val failProb: Double) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    if Math.random() > failProb then
      robot.act()
      println(s"Robot succeeded performing the action: ${robot.toString}")
    else
      println(s"Robot failed to perform the action (failure probability: ${failProb * 100}%)")

class RobotRepeated(val robot: Robot, val rep: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    for
      i <- 0 until rep
    do
      robot.act()
      println(robot.toString)

@main def testRobot(): Unit =
  import Direction.*
  val robot = LoggingRobot(SimpleRobot((0, 0), North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val batteryRobot = RobotWithBattery(SimpleRobot((0, 0), North))
  batteryRobot.act()
  batteryRobot.battery.getClass
  batteryRobot.act()
  batteryRobot.act()
  batteryRobot.act()
  batteryRobot.turn(batteryRobot.direction.turnRight)
  batteryRobot.act()
  batteryRobot.act()

  val canFailRobot = RobotCanFail(SimpleRobot((0, 0), North), 0.40)
  canFailRobot.act()
  canFailRobot.act()
  canFailRobot.act()

  val repeatedRobot = RobotRepeated(SimpleRobot((0, 0), North), 3)
  repeatedRobot.act()
  repeatedRobot.turn(repeatedRobot.direction.turnLeft)
  repeatedRobot.act()
