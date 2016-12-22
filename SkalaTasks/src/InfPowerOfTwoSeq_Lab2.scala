import Stream._

object InfPowerTwoSequence_Lab2 {
  
  
   //This function start calculate sequence from the first element (2 to the 0 power) 
   def firstPowerOfTwo = {
      powerOfTwoAccumulator(1, 2)
    }
  
    //This function will generate infinite sequence consisting of power of two numbers
    def powerOfTwoAccumulator(acc : Long, b : Long): Stream[Long] = {
      acc #:: powerOfTwoAccumulator(acc*b,b)
    }
    
    //Now the same task, but via unfold[T, S](z: S)(f: S => Option[(T, S)]): Stream[T]
    //Generate first element of sequence (2 to the 0 power)
    def unfoldFirstPowerOfTwo = {
      unfoldPowerOfTwo(1,2)
    }
    
    //Generate the same sequence like powerOfTwoAccumulator function, but via unfold
    def unfoldPowerOfTwo(accumulator : Long, power : Long) = {
      unfold(accumulator)(accumulator => Some(accumulator, accumulator*power))
    }
    
    //Unfold function. The first argument is initial state of the sequence,second - function with two arguments.
    //(first - next value of sequence, second - next value to be generated and added to stream). 
    def unfold[T, S](z: S)(f: S => Option[(T, S)]): Stream[T] = { 
      f(z) match {
        case Some((a,b)) =>  a #:: unfold(b)(f)
        case None => Empty
      }
    }
  
  def main(args: Array[String]) : Unit = {
    //Tests
    //Why we need to use constructions like func.take(num).print()?
    //Because stream generates infinite sequence, but calculate only the first value of it.
    //All next values will be calculate only when we refer to them. That is why I'm used take(num) method.
    //And, of course, we need to print them :-) That's what is print() for.
    println ( firstPowerOfTwo.take(5).print() )
    println ( firstPowerOfTwo.take(10).print() )
    println ( unfoldFirstPowerOfTwo.take(5).print() )
    println ( unfoldFirstPowerOfTwo.take(10).print() )
  } 
}

