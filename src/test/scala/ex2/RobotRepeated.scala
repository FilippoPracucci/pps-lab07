package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Direction.*

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  "A RobotRepeated" should "turn correctly" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), North), 3)

    robot.turn(East)
    robot.direction should be(East)
    robot.rep should be(3)

  it should "act correctly" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), North), 3)

    robot.act()
    robot.position should be((0, 3))

    robot.turn(East)
    robot.act()
    robot.position canEqual be((3, 3))

    robot.turn(South)
    robot.act()
    robot.direction should be(South)
    robot.position should be((3, 0))
