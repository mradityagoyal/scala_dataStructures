package com.goyal.addy.datastructures.wk1

import scala.collection.mutable.Stack
import scala.io.StdIn

object CheckBrackets extends App {

  def validateBrackets(str: String): String = {

    //returns true if left bracket closes right bracket. 
    def isClosingPair(l: Char, r: Char): Boolean = (l, r) match {
      case ('(', ')') | ('{', '}') | ('[', ']') => true
      case otherwise => false
    }

    val unmatchedBrackets: Stack[(Char, Int)] = new Stack
    for (idx <- 0 until str.length()) {
      val c = str(idx)
      c match {
        case '(' | '{' | '[' => unmatchedBrackets.push((c, idx))
        case ')' | '}' | ']' => {
          if (unmatchedBrackets.isEmpty ||
            !isClosingPair(unmatchedBrackets.pop()._1, c)) return (idx + 1).toString()
        }
        case otherwise =>
      }
    }
    if (unmatchedBrackets.isEmpty) "Success"
    else (unmatchedBrackets.pop()._2 + 1).toString()
  }

  override def main(args: Array[String]) = {

    val input = StdIn.readLine();
    val result = validateBrackets(input);
    println(result)
  }
}