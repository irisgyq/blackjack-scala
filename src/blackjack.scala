/**
  * Created by irisgyq on 2017/3/28.
  */

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.util.control.Breaks
object blackjack {


  def main(args: Array[String]): Unit = {

    var initcards: Array[Int] = Array(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10,
      10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)

    var playerSum: Int = 0
    var dealerSum: Int = 0

    var playerCards = new ListBuffer[Int]()
    var dealerCards = new ListBuffer[Int]()

    var PisBust: Boolean = false
    var DisBust: Boolean = false

    println("Game begins...")
    var cards = shuffle(initcards)
    var playerCard = cards.pop()
    playerCards.append(playerCard)
    println("The player's first card is:" + playerCard)
    var dealerCard = cards.pop()
    dealerCards.append(dealerCard)
    println("The dealer's fisrt card is:" + dealerCard)
    playerCard = cards.pop()
    playerCards.append(playerCard)
    println("The player's second card is:" + playerCard)

    if (isblackjack(playerCards.toArray)) {
      println("The player has blackjack!")
      println("Game is over, the player is the winner.")
    } else {
      dealerCard = cards.pop()
      dealerCards.append(dealerCard)

      if (isblackjack(dealerCards.toArray)) {
        println("The dealer's second card is: " + dealerCard)
        println("The dealer has blackjack!")
        println("Game is over, the dealer is the winner.")
      } else {
        playerSum = playerCards.head + playerCards.apply(1)
        dealerSum = dealerCards.head + dealerCards.apply(1)

        println("The sum of player's cards is: " + playerSum)

        var isPValid = true
        while (isPValid) {
          println("Does the player want one more card?")
          var input = scala.io.StdIn.readLine()

          input match {
            case "yes" => {
              playerCard = cards.pop()
              playerCards.append(playerCard)
              println("This card is: " + playerCard)
              playerSum += playerCard
              println("The sum of player's card is: " + playerSum)

              if (isblackjack(playerCards.toArray)) {
                println("The player has 21 points!")
                isPValid = false
                Breaks
              } else if (playerSum > 21) {
                println("Player's cards are busting.")
                PisBust = true
                isPValid = false
                Breaks
              }
              Breaks
            }

            case "hit" => {
              isPValid = false
              Breaks
            }
          }
        }

        println("The dealer's second card is: " + dealerCard)
        println("The sum of dealer's cards is: " + dealerSum)

        var isDValid = true
        while (isDValid) {
          while (dealerSum < 17) {
            println("Because the sum of dealer's cards is less than 17, he must add one more card.")
            dealerCard = cards.pop()
            dealerCards.append(dealerCard)
            println("The new card is:" + dealerCard)
            dealerSum += dealerCard
            println("The sum of dealer's cards is:" + dealerSum)

            if (dealerSum == 21) {
              println("The dealer has 21 points!")
              Breaks
            }
            if (dealerSum > 21) {
              println("dealer's cards are busting.")
              DisBust = true
              Breaks
            }
          }

          if (dealerSum < 21) {
            println("Does the dealer want one more card?")
            var input = scala.io.StdIn.readLine()
            input match {
              case "yes" => {
                dealerCard = cards.pop()
                dealerCards.append(dealerCard)
                println("This card is:" + dealerCard)
                dealerSum += dealerCard
                println("The sum of dealer's card is:" + dealerSum)

                if (isblackjack(dealerCards.toArray)) {
                  println("The dealer has 21 points!")
                  isDValid = false
                  Breaks
                } else if (dealerSum > 21) {
                  println("dealer's cards are busting.")
                  DisBust = true
                  isDValid = false
                  Breaks
                }
              }
              case "hit" => {
                isDValid = false
                Breaks
              }
            }
          } else {
            isDValid = false
          }

        }
        if ((DisBust && PisBust) || (!DisBust && !PisBust && (dealerSum == playerSum))) {
          println("Game is over, it's a push")
        } else if ((DisBust && !PisBust) || (!DisBust && !PisBust && (dealerSum < playerSum))) {
          println("Game is over, the player wins")
        } else if ((!DisBust && PisBust) || (!DisBust && !PisBust && (dealerSum > playerSum))) {
          println("Game is over, the dealer wins")
        }


      }
    }
  }

  def shuffle (a : Array[Int]) : Stack[Int]= {
    var temp = Stack[Int]()
    var list = a.toList
    var mylist = scala.util.Random.shuffle(list)
    temp.pushAll(mylist)
    temp
  }

  def isblackjack (a : Array[Int]) : Boolean = {
     var sum = 0
     var hasOne = false
     for (i <- a.indices) {
       sum += a(i)
       if (a(i) == 1) {
         hasOne = true
       }
     }

     if (sum == 21) {
       return true
     } else if (hasOne && sum+10==21) {
       return true
     }

     false
  }
}
