package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c==0 || c==r) 1
    else pascal(c-1,r-1) + pascal (c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def countPar(characters: List[Char], n:Int, m:Int):Boolean = {
      if (!characters.tail.isEmpty)
        if (characters.head.compare('(')==0) countPar(characters.tail,n+1,m)
        else if (characters.head.compare(')')==0) countPar(characters.tail,n,m+1)
        else countPar(characters.tail,n,m)
      else
        if (characters.head.compare('(')==0) n+1
        else if (characters.head.compare('(')==0) m+1
        (n==m)
    }
    if (chars.isEmpty) false
    else countPar(chars, 0, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
