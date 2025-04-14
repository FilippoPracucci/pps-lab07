package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  "A RobotWithBattery" should "turn correctly" in:
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North))

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    robot.battery should be(100)

  it should "act correctly" in:
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North))

    robot.act()
    robot.position should be((0, 1))
    robot.battery should be(80)

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))
    robot.battery should be(60)

    robot.act()
    robot.battery should be(40)

    robot.act()
    robot.battery should be(20)
    robot.act()
    robot.battery should be(0)

    a [IllegalStateException] should be thrownBy robot.act()
    robot.battery should be(0)
