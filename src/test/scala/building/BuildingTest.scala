package building

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BuildingTest extends AnyFlatSpec {

  import building.Building._

  val livingFloors: Floor = LivingFloor(
    Man(20), Man(40),
    LivingFloor(
      Man(17), Man(33),
      LivingFloor(
        Man(56), Man(25),
        LivingFloor(
          Woman(32), Woman(98),
          LivingFloor(
            Woman(31), Man(34),
            LivingFloor(
              Man(90), Woman(95),
              Attic
            )
          )
        )
      )
    )
  )

  val currentBuilding: Building =
    Building("Moscow, Black Sea, 44.933333, 31.733333", livingFloors)

  it should "5 men older then 30" in {
    countOldManFloors(currentBuilding, 30) should equal (5)
  }

  it should "max age woman is 98" in {
    womanMaxAge(currentBuilding) should equal (98)
  }

}
