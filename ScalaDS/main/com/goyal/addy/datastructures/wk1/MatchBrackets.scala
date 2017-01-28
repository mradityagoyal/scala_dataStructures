package com.goyal.addy.datastructures.wk1

import scala.collection.mutable.Stack

object MatchBrackets extends App {

  def validateBrackets(str: String): Boolean = {
    
    def isClosingPair(l: Char, r: Char): Boolean = (l, r) match {
      case ('(', ')') | ('{', '}') | ('[', ']') => true
      case otherwise => false
    }
    
    val unmatchedBrackets: Stack[Char] = new Stack
    for (c <- str) {
      c match {
        case '(' | '{' | '[' => unmatchedBrackets.push(c)
        case ')' | '}' | ']' => {
          if (unmatchedBrackets.isEmpty) return false
          if (!isClosingPair(unmatchedBrackets.pop(), c)) return false
        }
        case otherwise =>
      }
    }
    unmatchedBrackets.isEmpty
  }

  override def main(args: Array[String]) = {
    println(validateBrackets("  "))
    println(validateBrackets("  (("))
    println(validateBrackets("  (())"))
  }
}