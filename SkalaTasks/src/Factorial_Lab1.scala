object factorial {
  
  //Function to calculate factorial
  def factorial(number: Long) : Long = {
    //This function calculate factorial in the next way:
    //Until our number is not equal 1 - multiple it with accumulator and decrease number;
    //When number equal 1 - that mean we calculated factorial. Just return the accumulator.
    def factorialAccumulator(accumulator : Long, number : Long) : Long = {
      number match {
        case 1 => accumulator
        case _ => factorialAccumulator(accumulator * number, number-1)
      }
    }
  factorialAccumulator(1,number)
}
  
  def main(args: Array[String]) {
    //Tests
    println( factorial(5) )
    println( factorial(10) )
    println( factorial(20) )
  }
  
}