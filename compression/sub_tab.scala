package oscar.cp.compression

class sub_tab (val vrs:Array[Int], val vls: Array[Array[Int]], nActiveTup:Int){
  private var vars = vrs
  private var sub_tup = vls

  private var nActiveSubTup = nActiveTup
  //private var activeTuples = Array.tabulate(vls.length)(i => i)


  def set_vars(x: Array[Int]){
    vars= x
  }
  def get_vars(): Array[Int] ={
    return vars
  }


  def set_sub_tup(x: Array[Array[Int]]){
    sub_tup= x
  }
  def get_sub_tup(): Array[Array[Int]] ={
    return sub_tup
  }


  def set_nActiveSubTup(x: Int){
    nActiveSubTup= x
  }
  def get_nActiveSubTup(): Int ={
    return nActiveSubTup
  }


}
