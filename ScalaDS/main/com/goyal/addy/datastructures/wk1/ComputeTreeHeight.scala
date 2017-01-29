package com.goyal.addy.datastructures.wk1
import scala.io.StdIn
import scala.collection._
import scala.collection.mutable.ListBuffer

object ComputeTreeHeight extends App {

  override def main(args: Array[String]) {

    val numberOfNodes = StdIn.readInt()

    val secondLine = StdIn.readLine()
    //array of position of parents. 
    val parents: Array[Int] = secondLine.split(" ").map(_.toInt)

    val root = parseTree(numberOfNodes, parents)
    println(getHeightIterative(root))

  }
  //parse a tree and return the root node from the parentArray representation
  def parseTree(n: Int, parents: Array[Int]): Node = {
    var root: Node = null
    //    array of nodes. all null to start with. 
    val created: Array[Option[Node]] = Array.fill(n)(None)
    // create a node and it's parent
    def createNode(idx: Int): Unit = {
      //if node at index idx is not defined . create node and parent. 
      if (!created(idx).isDefined) {
        val newNode = new Node(idx)
        created(idx) = Some(newNode)
        //index of parent node 

        val parentIdx = parents(idx)
        if (parentIdx == -1) {
          root = newNode
        } else {
          created(parentIdx) match {
            //if parent node is found. add this new node as a child to it. 
            case Some(nd) => nd.addChild(newNode)
            //else create a parent node. 
            case None => created(parentIdx) = Some(new Node(parentIdx, ListBuffer(newNode)))
          }
        }
      } //else node is defined. just check it's parent is defined. 
      else {
        val parentIdx = parents(idx)
        if (parentIdx == -1) {
          //this is root node. 
          root = created(idx).get
        } else {
          val parentNode = created(parentIdx)
          parentNode match {
            //if parent node is found. add this new node as a child to it. 
            case Some(nd) => nd.addChild(created(idx).get)
            //else create a parent node. 
            case None => created(parentIdx) = Some(new Node(parentIdx, ListBuffer(created(idx).get)))
          }
        }
      }
    }

    for (i <- 0 until n) {
      createNode(i)
    }
    root
  }

  /*
   * returns the height of the tree given the root node. 
   * calculates the height iteratively
   */
  def getHeightIterative(root: Node): Int = {
    val q = mutable.Queue[Node]()
    q.enqueue(root)
    var height = 0
    while (true) {
      //      nodeCount(queue size) indicates number of nodes
      //      at current level
      var nodeCount = q.length
      if (nodeCount == 0) return height

      height += 1
      //      Dequeue all nodes of current level and Enqueue
      //        # all nodes of next level
      while (nodeCount > 0) {
        val nd = q.dequeue()
        nd.children.foreach(q.enqueue(_))
        nodeCount -= 1
      }
    }
    height
  }
}

class Node(val value: Int, val children: mutable.ListBuffer[Node] = ListBuffer.empty[Node]) {
  def addChild(child: Node) = children.+=(child)
  def height: Int = {
    if (children.isEmpty) 1
    else 1 + children.map(_.height).max
  }
}