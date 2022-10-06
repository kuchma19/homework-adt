package building

import scala.annotation.tailrec

sealed trait Person

case class Man(age: Int) extends Person

case class Woman(age: Int) extends Person


sealed trait Floor

case object Attic extends Floor

case class LivingFloor(firstPerson: Person, secondPerson: Person, nextFloor: Floor) extends Floor


case class Building(address: String, floors: Floor)

object Building {

  def protoFold(building: Building, acc0: Int)(f: (Int, LivingFloor) => Int): Int = {
    @tailrec
    def protoFoldHelper(currentFloor: Floor, acc: Int): Int = {
      currentFloor match
        case Attic => acc
        case livingFloor@LivingFloor(_, _, nextFloor)
        => protoFoldHelper(nextFloor, f(acc, livingFloor))
    }

    protoFoldHelper(building.floors, acc0)
  }

  def countOldManFloors(building: Building, olderThen: Int): Int =
    protoFold(building, 0) {
      (acc, floor) => {
        def isManOlderThen(person: Person): Boolean = {
          person match
            case Man(age) if age > olderThen => true
            case _ => false
        }

        acc +
          (if (isManOlderThen(floor.firstPerson) || isManOlderThen(floor.secondPerson)) 1
          else 0)
      }
    }

  def womanMaxAge(building: Building): Int = {
    val defaultWomanAge = 0
    protoFold(building, defaultWomanAge) {
      (acc, floor) => {
        def womanAgeOrDefault(person: Person): Int = {
          person match
            case Woman(age) => age
            case _ => defaultWomanAge
        }

        acc max womanAgeOrDefault(floor.firstPerson) max womanAgeOrDefault(floor.secondPerson)
      }
    }
  }


}
