package oscar.cp.compression

class entrie (val pt : pattern, val st: sub_tab,  val positionsVar: Array[Int], id_entr:Int) {
  private var pat = pt
  private var sub_t = st
  private var positionVars = positionsVar
  private var id_ent = id_entr
  private var tauxMch = 0.0


  def set_chauv(x: Double): Unit ={
    tauxMch = x
  }
  def get_chauv(): Double={
    tauxMch
  }
  def set_pat(x: pattern){
    pat = x
  }
  def get_pat(): pattern ={
    return pat
  }

  def set_sub_t(x: sub_tab){
    sub_t= x
  }
  def get_sub_t(): sub_tab ={
    return sub_t
  }

  def set_positionVars(x: Array[Int]){
    positionVars = x
  }
  def get_positionVars(): Array[Int] ={
    return positionVars
  }

  def set_id_ent(x: Int){
    id_ent = x
  }
  def get_id_ent(): Int ={
    return id_ent
  }
}
