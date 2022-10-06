package tree

sealed trait Tree

case class Node(value: Int, left: Tree, right: Tree) extends Tree

case object RedLeaf extends Tree
case object YellowLeaf extends Tree
case object GreenLeaf extends Tree


object Tree {
  def countYellowAndRedValues(tree: Tree): Int =
    tree match
      case Node(value, YellowLeaf | RedLeaf, right) => value + countYellowAndRedValues(right)
      case Node(value, left, YellowLeaf | RedLeaf) => value + countYellowAndRedValues(left)
      case Node(_, left, right) => countYellowAndRedValues(left) + countYellowAndRedValues(right)
      case _ => 0

  def maxValue(tree: Tree): Option[Int] =
    tree match
      case Node(value, left, right) =>
        val maxValueLeft = maxValue(left) getOrElse value
        val maxValueRight = maxValue(right) getOrElse value
        Some(value max maxValueLeft max maxValueRight)
      case _ => None
}