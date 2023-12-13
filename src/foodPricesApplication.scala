// Programming Paradigms Coursework Assignment
// Ewan Allison - December 2023

// Import required libraries

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Try

object foodPricesApplication extends App {

  // Function to read in data from .txt file

  def readFile(filename: String): Map[String, List[Int]] = {

    var itemMap: Map[String, List[Int]] = Map() // Initialise map to store food items and prices

    try {
      val fileSource = Source.fromFile(filename)
      for (line <- fileSource.getLines()) {
        val items = line.split(",").map(_.trim)
        if (items.length > 1) {
          val name = items(0) // Get food item name
          val prices = items.tail.flatMap(string => Try(string.toInt).toOption).toList // Get food item prices, ensuring integer values
          itemMap = itemMap + (name -> prices)  // Add food item to map
        }
      }
      fileSource.close()
    } catch {
      case ex: Exception => println("Sorry, an exception happened: " + ex) // Print error
    }

    itemMap // Return food items map
  }

  val foodPriceData = readFile("data.txt") // Read in and store food price data

  // Event handling for menu options

  val actionMap = Map[Int, () => Boolean](
    1 -> (() => { handleOne(); pauseForUserInput(); true }),
    2 -> (() => { handleTwo(); pauseForUserInput(); true }),
    3 -> (() => { handleThree(); pauseForUserInput(); true }),
    4 -> (() => { handleFour(); pauseForUserInput(); true }),
    5 -> (() => { handleFive(); pauseForUserInput(); true }),
    6 -> (() => { handleSix(); true}),
    7 -> handleSeven,
  )

  var opt = "" // Variable for menu choice

  do {
    opt = readOption
  } while (menu(opt))

  // Display menu and read in menu choice from user

  def readOption: String = {
    println(
      """Food Basket Prices Application
         |
         |  [1] - View current prices
         |  [2] - View highest/lowest prices
         |  [3] - View median prices
         |  [4] - View highest riser
         |  [5] - Compare average prices over 2 year period
         |  [6] - Calculate food basket
         |  [7] - Exit application""".stripMargin)

    println("\nEnter your menu choice: ")
    scala.io.StdIn.readLine() // Read in menu choice from user
  }

  // Validate menu option user input and use actionMap route request

  def menu(option: String): Boolean = {

    option.trim match {
      case "" =>
        println("No input detected. Please enter a valid command.\n") // Output error for no user input
        true
      case str if str.forall(_.isDigit) =>
        val optInt = str.toInt
        actionMap.get(optInt) match {
          case Some(f) => f()
          case None =>
            println("Sorry, that command is not recognized\n") // Output error for out-of-bounds user input
            true
        }
      case _ =>
        println("Invalid input. Please enter a number.\n") // Output error for nonsensical user input
        true
    }

  }

  // Allow user to view output before returning to menu

  def pauseForUserInput(): Unit = {
    println("Press Enter to return to menu...")
    readLine() // Waits for the user to press Enter
  }

  // Pure function, returns integer in pounds and pence

  def formatPriceInPounds(priceInPence: Int): String = {
    val pounds = priceInPence / 100
    val pence = priceInPence % 100
    f"£$pounds%1d.$pence%02d"
  }

  // Pure function, creates a separator line to format output tables

  def createSeparator(length: Int): String = {
    val separator = "-" * length
    separator
  }

  // Pure function, creates a header from input parameters
  def formatHeader(headers: Seq[String]): String = {
    // ANSI escape code for bold text
    val boldStart = "\u001B[1m"
    val boldEnd = "\u001B[0m"

    // Create and return header
    headers.map(header => boldStart + f"$header%-15s" + boldEnd).mkString(" | ")
  }

  // Handlers for menu options

  // Function which finds the most recent price of each food item

  def getMostRecentPrice(): String = {

    // Fold left over the price data

    val result = foodPriceData.foldLeft (formatHeader (Seq ("Food Item", "Most Recent Price") ) + "\n") {
    case (acc, (food, prices) ) =>
    val formattedFood = food.toLowerCase ().capitalize // Format text
    val mostRecentPrice = formatPriceInPounds (prices.last) // Get most recent price
    acc + f"$formattedFood%-15s | $mostRecentPrice\n" // Append the current line to the accumulator
    }

    result // Return result

  }

  // Menu option 1, return the most recent prices

  def handleOne(): Boolean = {

    println(getMostRecentPrice()) // Print result
    true

  }

  def getHighestLowestPrice(): String = {

    // Fold left over the price data

    val result = foodPriceData.foldLeft(formatHeader(Seq("Food Item", "Highest Price", "Lowest Price")) + "\n") {
      case (acc, (food, prices)) =>
        val maxPrice = formatPriceInPounds(prices.max) // Get highest price
        val minPrice = formatPriceInPounds(prices.min) // Get lowest price
        acc + f"$food%-15s | $maxPrice%-15s | $minPrice\n" // Append the current line to the accumulator
    }

    result // Return result
  }

  // Menu option 2, return the highest and lowest prices

  def handleTwo(): Boolean = {

    println(getHighestLowestPrice()) // Print the result
    true

  }

  // Pure function that finds median of integer list

  def median(prices: List[Int]): Double = {

    // Sort numbers smallest to highest
    val sortedPrices = prices.sorted
    val n = sortedPrices.length

    if (n % 2 != 0) {
      // Odd number of elements, return the middle
      sortedPrices(n / 2)
    } else {
      // Even number of elements, return the average of the two middle
      val (upperMid, lowerMid) = (sortedPrices(n / 2), sortedPrices(n / 2 - 1))
      (upperMid + lowerMid) / 2.0
    }

  }

  def getMedianPrice(): String = {

    // Fold left over the price data

    val result = foodPriceData.foldLeft(formatHeader(Seq("Food Item", "Median Price")) + "\n") {
      case (acc, (food, prices)) =>
        val medianPrice = median(prices) // Find the median prices
        acc + f"$food%-15s | ${formatPriceInPounds(medianPrice.toInt)}\n" // Append the current line to the accumulator
    }

    result // Return result
  }

  // Menu option 3, return the median prices

  def handleThree(): Boolean = {

    println(getMedianPrice()) // Print the result
   true

  }

  // Pure function that finds average

  def average(prices: List[Int]): Double = {

    if (prices.isEmpty) 0.0
    else prices.sum / prices.length.toDouble

  }

  def getHighestRiser(): String = {

    // Fold right over food price data

    val priceRise = foodPriceData.foldRight(Map.empty[String, (Double, Double)]) {
      case ((food, prices), acc) =>
        val last6MonthsAverage = average(prices.takeRight(6)) // Find average of last 6 months
        val previous6MonthsAverage = average(prices.dropRight(6).takeRight(6)) // Find average of the 6 months before then
        val rise = last6MonthsAverage - previous6MonthsAverage // Calculate the rise in price value
        val percentageIncrease = if (previous6MonthsAverage != 0) (rise / previous6MonthsAverage) * 100 else 0 // Calculate the percentage increase
        acc + (food -> (rise, percentageIncrease))
    }

    // Find the item with the highest rise
    val highestRiser = priceRise.maxBy(_._2._1)
    val (priceRiseAmount, percentage) = highestRiser._2
    val result = formatHeader(Seq("Food item","Price rise","Percentage")) + f"\n${highestRiser._1}%-15s | ${formatPriceInPounds(priceRiseAmount.toInt)}%-15s | ${"%.2f".format(percentage)}\n"

    result // Return result
  }

  // Menu option 4, return highest rising price

  def handleFour(): Boolean = {

    println(getHighestRiser()) // Print the result
    true
  }

  // Tail recursive function for getting valid food item

  @tailrec
  def getValidFoodItem(prompt: String): String = {

    println(prompt) // Print prompt
    val input = scala.io.StdIn.readLine().toUpperCase() // Read in food item

    if (foodPriceData.contains(input)) input
    else {
      println(s"Error: '$input' is not a valid food item. Please try again.") // Output error
      getValidFoodItem(prompt) // Recursive call for valid input
    }

  }

  def compareTwoItems(): Unit = {

    // Read in food items to compare
    val food1 = getValidFoodItem("Enter the first food item:")
    val food2 = getValidFoodItem("Enter the second food item:")

    // Calculate and display the averages of both items
    val avg1 = average(foodPriceData(food1).takeRight(24))
    val avg2 = average(foodPriceData(food2).takeRight(24))

    // Print averages of both items
    println(s"\nAverage price for ${food1.toLowerCase()} over the last 2 years was ${formatPriceInPounds(avg1.toInt)}")
    println(s"Average price for ${food2.toLowerCase()} over the last 2 years was ${formatPriceInPounds(avg2.toInt)}\n")

  }

  // Menu option 5, compare 2 year averages of two food items

  def handleFive(): Boolean = {

    println(getMostRecentPrice()) // Display all food items for UX ease
    compareTwoItems()

    true
  }

  var basket = Map[String, Float]() // Initialise basket

  def displayBasket(): Unit = {

    println("\nYour Basket\n")

    if (basket.isEmpty) {
      println("Basket is currently empty.") // Show if basket is empty
    } else {
      println(formatHeader(Seq("Food item","Quantity (kg/l)","Price"))) // Get formatted header
      var total = 0.0
      basket.foreach { case (food, quantity) =>
        val price = foodPriceData(food).last * quantity // Calculate items total price using quantity
        total += price // Calculate running total
        val formattedPrice = formatPriceInPounds(price.toInt) // Format total in pounds and pence
        println(f"$food%-15s | $quantity%-15s | $formattedPrice") // Print basket items, prices, and quantities
      }

      val formattedTotal = formatPriceInPounds(total.toInt)
      println(f"\nBasket Total = $formattedTotal") // Print total price of basket

    }
  }


  def addItemToBasket(): Unit = {

    handleOne() // Display food items and prices
    val food = getValidFoodItem("Enter the food item:") // Read in food item
    println("Enter the quantity (kg/litres):") // Read in quantity

    // Try to convert user input to a floating-point number.
    Try(readLine().toFloat).toOption match {
      case Some(quantity) =>
        basket = basket + (food -> (basket.getOrElse(food, 0f) + quantity)) // Add food + quantity to basket
        println(s"$food added to the basket.") // Print success
      case None =>
        println("Invalid quantity. Please enter a valid number.") // Print failure
    }

  }

  @tailrec
  def basketInteraction(): Unit = {

    displayBasket() // Print current basket
    println("\n  [1] Add item to basket\n  [2] Exit basket\n\nEnter your menu choice:") // Print menu
    val option = readLine() // Read in menu choice

    option match {
      case "1" =>
        addItemToBasket() // Call for adding items
        basketInteraction() // Recursive call for further interaction
      case "2" => println("Exiting basket.")
      case _ =>
        println("Invalid option. Please try again.") // Print error
        basketInteraction() // Recursive call for valid option
    }

  }

  def handleSix(): Boolean = {

    basket = Map() // Reset basket
    basketInteraction() // Start basket interaction

    true
  }

  def handleSeven(): Boolean = {

    println("goodbye!") // Returns false so loop terminates

    false
  }

}