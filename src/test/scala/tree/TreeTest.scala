package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TreeTest extends AnyFlatSpec {

  import tree.Tree._

  "countYellowAndRedValues" should "the sum in leafs is zero" in {
    countYellowAndRedValues(RedLeaf) should equal (0)
    countYellowAndRedValues(GreenLeaf) should equal (0)
    countYellowAndRedValues(YellowLeaf) should equal (0)
  }

  it should "in a tree with green leaves the sum is 0" in {
    val tree = Node(20,
      Node(10, GreenLeaf, Node(15, GreenLeaf, GreenLeaf)),
      Node(4, Node(17, GreenLeaf, GreenLeaf), Node(1, GreenLeaf, Node(2, GreenLeaf, GreenLeaf)))
    )
    countYellowAndRedValues(tree) should equal (0)
  }

  val justRandomTree: Node = Node(4,
    Node(10,
      GreenLeaf,
      Node(15,
        GreenLeaf,
        GreenLeaf)
    ),
    Node(4,
      Node(17,
        RedLeaf,
        Node(12, YellowLeaf, GreenLeaf)
      ),
      Node(1,
        GreenLeaf,
        Node(2,
          YellowLeaf,
          GreenLeaf
        )
      )
    )
  )

  it should "the sum of values in the nodes with at least one of the 2 child elements is a yellow or red leaf" in {
    countYellowAndRedValues(justRandomTree) should equal (17 + 12 + 2)
  }

  "maxValue" should "in leaf max value is None" in {
    maxValue(RedLeaf) should equal (None)
  }

  it should "max value in tree" in {
    maxValue(justRandomTree) should equal (Some(17))
  }


}
