package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    pascalAux(new Array(1), c, r)
  }

  def pascalAux(triangle: Array[Int], c: Int, r: Int): Int = {
    if(r == 0) triangle(c)
    else {
      val previousLength = triangle.length
      var t : Array[Int] = new Array[Int](previousLength+1)
      t(0) = 1
      t(previousLength) = 1
      for(a <- 1 to previousLength-1){
        t(a) = triangle(a-1) + triangle(a)
      }
      pascalAux(t, c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceAux(chars, 0)
  }

  def balanceAux(chars: List[Char], parantheses: Int): Boolean = {
    if(chars.isEmpty && parantheses==0) true
    else if (chars.isEmpty && parantheses!=0) false
    else if(chars.head == '('){
      balanceAux(chars.tail, parantheses+1)
    }else if(chars.head == ')'){
      if(parantheses == 0) false
      else {
        balanceAux(chars.tail, parantheses-1)
      }
    }else {
      balanceAux(chars.tail, parantheses)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    countChangeAux(money, coins)
  }

  def countChangeAux(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty) {
      if(money == 0) 1
      else 0
    }
    else  {
      var use = 0
      var sol = 0
      while(money - coins.head * use >= 0 ){
        sol = sol +  countChangeAux(money - coins.head * use, coins.tail)
        use = use + 1
      }
      sol
    }
  }
}