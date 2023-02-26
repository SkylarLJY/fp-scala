package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if c==0 || r==0 || (r==c) then 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    var openNum = 0
    def helper(chars: List[Char]): Boolean = 
      if chars.isEmpty then openNum==0
      else if chars.head == ')' then
        openNum -= 1
        if openNum < 0 then return false
        helper(chars.tail)
      else if chars.head == '(' then 
        openNum += 1
        helper(chars.tail)
      else helper(chars.tail)
    helper(chars)
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if money<0 || coins.isEmpty then 0
    else if money==0 then 1 
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)