package oscar.cp.compression

class pattern (val vr:Array[Int], val vl: Array[Int]){
private var vars = vr
private var vals = vl

  def set_vars(x: Array[Int]){
    vars = x
  }
  def get_vars(): Array[Int] ={
    vars
  }

  def set_vals(x: Array[Int]){
    vals = x
  }
  def get_vals(): Array[Int] ={
    vals
  }

}
