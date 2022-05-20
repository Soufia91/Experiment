package oscar.cp.compression

import scala.collection.mutable.ArrayBuffer

class FiltredCmpressedTab {
  private var valideEntries: ArrayBuffer[Int] = new ArrayBuffer[Int]
  private var Tp: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]
  private var PositionsTable: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private var Ts: ArrayBuffer[Array[Array[Int]]] = new ArrayBuffer[Array[Array[Int]]]
  private var SubTablesVars: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val activeSubTuples: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val filteredTablee: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private val T: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  private var activeDefTuples: ArrayBuffer[Int] = new ArrayBuffer[Int]()
  private val nActiveSubTups: ArrayBuffer[Int] = new ArrayBuffer[Int]()


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
    SubTablesVars = x
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
}
