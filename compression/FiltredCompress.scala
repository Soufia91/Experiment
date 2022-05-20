package oscar.cp.compression
import scala.collection.mutable.ArrayBuffer

class FiltredCompress {
  private var stat_motif : ArrayBuffer[Int] =  new ArrayBuffer[Int]
  private var stat_defTable : ArrayBuffer[Int] =  new ArrayBuffer[Int]
  private var valideEntries: ArrayBuffer[Int] = new ArrayBuffer[Int]
  private var Tp: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]
  private var PositionsTable: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private var Ts: ArrayBuffer[Array[Array[Int]]] = new ArrayBuffer[Array[Array[Int]]]
  private var SubTablesVars: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private var patVars: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val activeSubTuples: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val filteredTablee: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val T: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private var activeDefTuples: ArrayBuffer[Int] = new ArrayBuffer[Int]()
  private val nActiveSubTups: ArrayBuffer[Int] = new ArrayBuffer[Int]()
  private val shortSubTable: ArrayBuffer[ Array[Array[Int]]] = new ArrayBuffer[Array[Array[Int]]]()
  private val shortPat: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val shortDefTable: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()

  def set_patVars(x: ArrayBuffer[Array[Int]]){
    patVars.addAll(x)
  }
  def get_patVars():Array[Array[Int]] ={
    patVars.toArray
  }
  def set_nActiveSubTups(x:  Array[Int]  ): Unit={
    nActiveSubTups.addAll(x)
  }
  def get_nActiveSubTups():  Array[Int]  ={
    nActiveSubTups.toArray
  }

  def set_T(x: Array[Array[Int]] ){
    T.addAll(x)
  }
  def get_T(): Array[Array[Int]] ={
    T.toArray
  }

  def set_filteredTablee(x: Array[Array[Int]] ){
    filteredTablee.addAll(x)
  }
  def get_filteredTablee(): Array[Array[Int]] ={
    filteredTablee.toArray
  }

  def set_valideEntries(x: ArrayBuffer[Int] ){
    valideEntries = x
  }
  def get_valideEntries(): Array[Int] ={
    valideEntries.toArray
  }
  def set_stat_defTable(x: ArrayBuffer[Int] ){
    stat_defTable = x
  }
  def increment_stat_defTable(pos: Int){
    stat_defTable(pos) += 1
  }
  def get_stat_defTable(): Array[Int] ={
    stat_defTable.toArray
  }
  def set_stat_motif(x: ArrayBuffer[Int] ){
    stat_motif = x
  }
  def increment_stat_motif(pos: Int){
    stat_motif(pos) += 1
  }
  def get_stat_motif(): Array[Int] ={
    stat_motif.toArray
  }

  def set_Tp(x: ArrayBuffer[Array[Int]]){
    Tp = x
  }
  def get_Tp():Array[Array[Int]] ={
    Tp.toArray
  }

  def set_PositionsTable(x: ArrayBuffer[Array[Int]]){
    PositionsTable = x
  }
  def get_PositionsTable(): Array[Array[Int]] ={
    PositionsTable.toArray
  }

  def set_Ts(x: ArrayBuffer[Array[Array[Int]]]){
    Ts = x
  }
  def get_Ts(): Array[Array[Array[Int]]]={
    Ts.toArray
  }

  def set_SubTablesVars(x: ArrayBuffer[Array[Int]]){
    SubTablesVars.addAll(x)
  }
  def get_SubTablesVars(): Array[Array[Int]] ={
    SubTablesVars.toArray
  }

  def set_activeSubTuples(x: Array[Array[Int]]){
    activeSubTuples.addAll(x)
  }
  def get_activeSubTuples(): Array[Array[Int]] ={
    activeSubTuples.toArray
  }


  def set_activeDefTuples(x: ArrayBuffer[Int]){
    activeDefTuples = x
  }
  def get_activeDefTuples(): ArrayBuffer[Int] ={
    activeDefTuples
  }


  /*def set_shortSubTable(sub_t:  Array[Array[Int]], _star: Int): Unit={
    val shortTable = {
      val buf = new Array[Int](sub_t(0).length)
      var nb = 0
      Array.tabulate(sub_t.length) { i =>
        nb = 0
        val tuple = sub_t(i)
        var j = sub_t(0).length
        while (j > 0) {
          j -= 1
          if (tuple(j) != _star) {
            buf(nb) = j
            nb += 1
          }
        }
        val shortTuple = new Array[Int](nb)
        System.arraycopy(buf, 0, shortTuple, 0, nb)
        shortTuple
      }
    }
    shortSubTable.addOne(shortTable)
  }
  def get_shortSubTable(): Array[Array[Array[Int]]]={
    shortSubTable.toArray
  }
  def set_shorPat(pat:  Array[Int], _star: Int): Unit={
    val shortTable = {
      val buf = new Array[Int](pat.length)
      var nb = 0
      var j = pat.length
      while (j > 0) {
        j -= 1
        if (pat(j) != _star) {
          buf(nb) = j
          nb += 1
        }
      }
      val shortTuple = new Array[Int](nb)
      System.arraycopy(buf, 0, shortTuple, 0, nb)
      shortTuple
    }
    shortPat.addOne(shortTable)
  }

  def get_shortPat():Array[Array[Int]]={
    shortPat.toArray
  }

  def set_shortDefTable(sub_t:  Array[Array[Int]], _star: Int): Unit={
    val shortTable = {
      val buf = new Array[Int](sub_t(0).length)
      var nb = 0
      Array.tabulate(sub_t.length) { i =>
        nb = 0
        val tuple = sub_t(i)
        var j = sub_t(0).length
        while (j > 0) {
          j -= 1
          if (sub_t(i) != _star) {
            buf(nb) = j
            nb += 1
          }
        }
        val shortTuple = new Array[Int](nb)
        System.arraycopy(buf, 0, shortTuple, 0, nb)
        shortTuple
      }
    }
    shortDefTable.addAll(shortTable)
  }
  def get_shortDefTable(): Array[Array[Int]]={
    shortDefTable.toArray
  }
*/

}
