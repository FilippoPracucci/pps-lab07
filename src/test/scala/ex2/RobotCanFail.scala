package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Direction.*

class RobotCanFailSpec extends AnyFlatSpec with Matchers:
  "A RobotCanFail" should "turn correctly" in:
    val robot = RobotCanFail(SimpleRobot((0, 0), North), 0.30)

    robot.turn(East)
    robot.direction should be(East)
    robot.failProb should be(0.30)

  it should "act correctly" in:
    val failProb = 0.30
    var robot = RobotCanFail(SimpleRobot((0, 0), North), failProb)

    robot.act()
    robot.position canEqual be((0, 1))

    robot = RobotCanFail(SimpleRobot((0, 1), North), failProb)
    robot.turn(East)
    robot.act()
    robot.position canEqual be((1, 1))

    robot = RobotCanFail(SimpleRobot((1, 1), East), failProb)
    robot.act()
    robot.position canEqual be((2, 1))
