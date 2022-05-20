package oscar.cp.compression

class CombinatorOfTwoInts(val maxLefValue: Int, val maxRightVal:Int, var maxLeftValue:Int, var maxRightValue:Int, var offset : Int,  var maxPossibleCombinedValue: Long){
  //var maxLeftValue:Int
  //var maxRightValue:Int
  //var offset : Int
  //var maxPossibleCombinedValue: Long
  this.maxLeftValue = maxLefValue
  this.maxRightValue = maxRightVal
  this.offset = maxRightVal + 1
 // val b
  this.maxPossibleCombinedValue = (BigInt.apply(maxLefValue).bigInteger.multiply(BigInt.apply(offset).bigInteger)).add(BigInt.apply(maxRightVal).bigInteger).longValue()
  //this.maxPossibleCombinedValue = b.longValue
  //def combinatorOfTwoInts(maxLefValue: Int, maxRightVal:Int): Unit ={
    //this.maxLeftValue = maxLefValue
    //this.maxRightValue = maxRightVal
   // this.offset = maxRightVal + 1
    //val b  : BigInt = (BigInt.apply(maxLefValue).bigInteger.multiply(BigInt.apply(offset).bigInteger)).add(BigInt.apply(maxRightVal).bigInteger)
    //this.maxPossibleCombinedValue = b.longValue}

  def combineIntValueFor( maxLefValue: Int, maxRightVal:Int):Int={
  assert (0 <= maxLefValue && maxLefValue <= maxLeftValue && 0 <= maxRightVal && maxRightVal <= maxRightValue && maxPossibleCombinedValue <= Int.MaxValue)
    return maxLefValue * offset + maxRightVal
  }
}
