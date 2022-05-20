package oscar.cp.compression

import oscar.cp.core.variables.CPIntVar

import java.io.FileWriter
import java.nio.file.{Files, Paths}
import java.util.stream.IntStream
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class FP_TreeLec( val tuples: Array[Array[Int]], val variables: Array[CPIntVar]) {
  val domains = Array.tabulate(variables.length)(i => variables(i).max)
  var combinator: CombinatorOfTwoInts = new CombinatorOfTwoInts(domains.length, domains.reduceLeft(_ max _), 0, 0, 0, 0)
  val  patterns: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
  val SUPPORT_THRESHOLD =2
  def MinerFPTree(): compressed_table = {
    val t1 =  System.currentTimeMillis()
   /* if(Files.exists(Paths.get(tuples.length+"M.txt")))
    {
      import java.io.PrintWriter
      var pw = new PrintWriter(tuples.length+"M.txt")
      pw.close()
      pw = new PrintWriter(tuples.length+"T.txt")
      pw.close()

    }*/
  if (Files.exists(Paths.get(tuples.length + "M.txt"))) {
    var id_ent = 0
    val vars = Array.tabulate(variables.size)(i => variables(i).name)
    val list_ent: ArrayBuffer[entrie] = new ArrayBuffer[entrie]
    for (line <- Source.fromFile(tuples.length+"M.txt").getLines()) {
      val inter = line.split(" : ")
      val var_val = inter(0).split(" ")
      val vr : ArrayBuffer[Int] = new ArrayBuffer[Int]()
      val vl : ArrayBuffer[Int] = new ArrayBuffer[Int]()
      for(prc<-0 until var_val.length){
        val item = var_val(prc).split("=")
        vr.addOne(variables(item(0).toInt).name.substring(1).toInt)
        vl.addOne(item(1).toInt)
      }
      val pattern = new pattern(vr.toArray, vl.toArray)
      val cover = inter(1).split(" ")
      val scope_subt: ArrayBuffer[Int] = new ArrayBuffer[Int]()
      for (i <- 0.until(tuples(0).size)) {
        if (!vr.contains(variables(i).name.substring(1).toInt)) {
          scope_subt.addOne(vars(i).substring(1).toInt)
        }
      }
      val sub_tb = Array.tabulate(cover.length, scope_subt.length) { case (i, j) => tuples(cover(i).toInt)(vars.indexOf("V" + scope_subt(j))) }
      val sub_table = new sub_tab(scope_subt.toArray, sub_tb, sub_tb.length)
      val positionsVar = Array.ofDim[Int](tuples(0).size)
      for (i <- 0.to(pattern.vr.size - 1)) {
        positionsVar(vars.indexOf("V" + pattern.vr(i))) = (-i - 1)
      }
      for (i <- 0.to(scope_subt.size - 1)) {
        positionsVar(vars.indexOf("V" + scope_subt(i))) = (i + 1)
      }
      val entry: entrie = new entrie(pattern, sub_table, positionsVar, id_ent)
      id_ent += 1
      //taux +=(entry.get_pat().get_vars().length+(entry.get_sub_t().get_sub_tup().length*entry.get_sub_t().get_vars().length))
      list_ent.addOne(entry)
    }
    //sort2(list_ent, 0,  list_ent.length - 1)
    //ordonnerChauv(list_ent)
    val defaultTable: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
    if(new java.io.File(tuples.length+"T.txt").isFile)
    {
      for (line <- Source.fromFile(tuples.length+"T.txt").getLines()) {
        val tup =  line.split(" ").map(_.toInt)
        defaultTable.addAll(Array.tabulate(tup.size)(i => tuples(tup(i))))
      }
    }
    val compressed_tab = new compressed_table(list_ent.toArray, defaultTable.toArray)
    /*val table_ori= reconstruction(compressed_tab)
    val list = table_ori.toList
    val list_tup : ArrayBuffer[List[Int]] = new ArrayBuffer[List[Int]]
    for(i <- 0 until(tuples.length)) {
      {
        list_tup.addOne(tuples(i).toList)
      }
    }
   // println("list_tuo: "+list_tup.length+" listori: "+table_ori.length)
   val indices : ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val liste = list_tup.toList
    val list_tupl : ArrayBuffer[List[Int]] = new ArrayBuffer[List[Int]]
    for(i <- 0 until(list.length))
    {
      if(liste.contains(list(i))){
        list_tupl.addOne(list(i))
       indices.addOne(liste.indexOf(list(i)))
      }
    }
    println("taille rec: "+indices.toSet.size)
    println("table_or: "+tuples.length+"  table_rec: "+table_ori.length+" sous: "+list_tupl.length)*/
    compressed_tab
  }
  else {


    val list_tuples = ArrayBuffer.tabulate(tuples.length)(i => i)
    val valueFrequencies = computeFrequencyOfValues(domains)

    var tail_moy = 0
    var freq_moy = 0
    val tree: FP_Tre = new FP_Tre()
    var j = 0
    for (tuple <- tuples) {
      val transaction: ArrayBuffer[Int] = new ArrayBuffer[Int]
      for (i <- 0.to(tuple.length - 1)) {
        val value = combinator.combineIntValueFor(i, tuple(i))
        if (valueFrequencies(value) >= SUPPORT_THRESHOLD)
          transaction.addOne(value)
      }
      sort(transaction, 0, transaction.length - 1, valueFrequencies)
      tree.addTransaction(transaction)
      j += 1
    }
    // val FrequentItemsets: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]
    val prefix: ArrayBuffer[Int] = new ArrayBuffer[Int]()
   val fw = new FileWriter(tuples.length + "M.txt", true);
    collectFrequentPatterns(tree.rootNode, prefix)
    //println("Nombre de motifs: "+patterns.length)
    sort(patterns, 0, patterns.length - 1)
    val selectedItems = normalize(patterns)
    sort(selectedItems, 0, selectedItems.length - 1)
    var id_ent = 0
    val list_ent: ArrayBuffer[entrie] = new ArrayBuffer[entrie]
    val vars = Array.tabulate(variables.size)(i => variables(i).name)
    for (itemset <- patterns) {
     // println("it: "+itemset.length)
      if (itemset.length > 2) {

        val vr = Array.tabulate(itemset.size)(k => variables(itemset(k) / (domains.reduceLeft(_ max _) + 1)).name.substring(1).toInt)
        val vl = Array.tabulate(itemset.size)(k => itemset(k) % (domains.reduceLeft(_ max _) + 1))

        val pat: pattern = new pattern(vr, vl)
        val scope_subt: ArrayBuffer[Int] = new ArrayBuffer[Int]()
        for (i <- 0.until(tuples(0).size)) {
          if (!vr.contains(variables(i).name.substring(1).toInt)) {
            scope_subt.addOne(vars(i).substring(1).toInt)
          }
        }
        val pos = Array.ofDim[Int](itemset.size)
        val positionsVar = Array.ofDim[Int](tuples(0).size)
        for (i <- 0.to(pat.vr.size - 1)) {
          positionsVar(vars.indexOf("V" + pat.vr(i))) = (-i - 1)
          pos(i) = vars.indexOf("V" + pat.vr(i))
        }
        val couverture: ArrayBuffer[Int] = new ArrayBuffer[Int]()
        // println(list_tuples.length)
        for (i <- 0 until (list_tuples.length)) {
          var exist: Boolean = true
          var j = 0
          while ((j < pos.length) && (exist)) {
            // println(vl(j)+":"+tuples(list_tuples(i))(pos(j)))
            if (vl(j) != tuples(list_tuples(i))(pos(j)))
              exist = false
            j += 1
          }
          if (exist)
            couverture.addOne(list_tuples(i))
        }
        if (couverture.length >= SUPPORT_THRESHOLD) {
          for (i <- 0.to(scope_subt.size - 1)) {
            positionsVar(vars.indexOf("V" + scope_subt(i))) = (i + 1)
          }
          var tab = ""
          for (vrl <- 0 until (vl.length)) {
            val vs = vars.indexOf("V" + vr(vrl)).toString + "=" + vl(vrl).toString + " "
            tab += vs
          }
          tab += " : " + couverture.mkString(" ")
          fw.write(tab + "\n")
          tail_moy += vl.length
          freq_moy += couverture.length
          val sub_tb = Array.tabulate(couverture.length, scope_subt.length) { case (i, j) => tuples(couverture(i))(vars.indexOf("V" + scope_subt(j))) }
          val sub_table = new sub_tab(scope_subt.toArray, sub_tb, sub_tb.length)
          val entry: entrie = new entrie(pat, sub_table, positionsVar, id_ent)
          id_ent += 1
          list_ent.addOne(entry)
          list_tuples --= couverture
        }
      }
    }
    //println("Nombre de frag: "+list_ent.length)
   fw.close()
    // sort2(list_ent,0,list_ent.length-1)
    val defaultTable = Array.tabulate(list_tuples.size)(i => tuples(list_tuples(i)))
    //val fww = new FileWriter(tuples.length + "T.txt", true);
    //fww.write(list_tuples.mkString(" "))
    //fww.close()
    //println("Nbr itemsets: "+patterns.length+"  nbr frag: "+list_ent.length+" default_table: "+defaultTable.length)
    var taille = 0
    taille += defaultTable.length * variables.length
    val tuplesComp = freq_moy
if(list_ent.length>0)
  {
    for (m <- list_ent) {
      taille += m.get_sub_t().get_sub_tup().length * m.get_sub_t().get_vars().length
    }

    tail_moy = tail_moy / list_ent.length

    freq_moy = freq_moy / list_ent.length
  }


    val comp = new compressed_table(list_ent.toArray, defaultTable)
    val fc = new FileWriter(tuples.length + "Comp.txt", true)
    val entete = "Smin;taille;Nb_tp_cmp;taux_cmp;Nbr_mot;tail_mpy_mot;freq_moy"
    val stat = tuples.length+";"+tuplesComp+";"+(100 -((taille*100)/(tuples.length*variables.length))+";"+comp.get_list_entries().length+";"+tail_moy+";"+freq_moy)

 //   val stat = "taille: " + tuples.length + "\n" + "Nb_tp_cmp " + tuplesComp  + "\ntaux_cmp: " + (100 - ((taille * 100) / (tuples.length * variables.length)) + "\nNbr_mot: " + comp.get_list_entries().length + "\ntail_mpy_mot: " + tail_moy + "\nfreq_moy: " + freq_moy+"\n\n")
   println(stat)
    fc.write(stat)
    fc.close()

    /*val table_ori= reconstruction(compressed_tab)
val list = table_ori.toList
val list_tup : ArrayBuffer[List[Int]] = new ArrayBuffer[List[Int]]
for(i <- 0 until(tuples.length)) {
  {
    list_tup.addOne(tuples(i).toList)
  }
}
// println("list_tuo: "+list_tup.length+" listori: "+table_ori.length)
val indices : ArrayBuffer[Int] = new ArrayBuffer[Int]()
val liste = list_tup.toList
val list_tupl : ArrayBuffer[List[Int]] = new ArrayBuffer[List[Int]]
for(i <- 0 until(list.length))
{
  if(liste.contains(list(i))){
    list_tupl.addOne(list(i))
   indices.addOne(liste.indexOf(list(i)))
  }
}
println("taille rec: "+indices.toSet.size)
println("table_or: "+tuples.length+"  table_rec: "+table_ori.length+" sous: "+list_tupl.length)*/
    val t2 = System.currentTimeMillis() - t1
    //print("\n temps de compression: "+t2)
    comp
  }
}





  def isPrefix(t1: Array[Int], t2: Array[Int]):Boolean={
    t1.length <= t2.length && IntStream.range(0,t1.length).noneMatch(i => t1(i) != t2(i))
  }

  def normalize(patterns: ArrayBuffer[Array[Int]]):ArrayBuffer[Array[Int]]={
    val keepIt: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]
    for(i <- 0 until patterns.length){
      val pattern = patterns(i)
      if(pattern.length>1){
        var subsumed : Boolean =false
        var j = i+1
        while (((j < patterns.length)) && (!subsumed)){
          if(isPrefix(pattern, patterns(j))){
            subsumed =true
          }
          j+=1
        }
        if(!subsumed){
          keepIt.addOne(pattern)
        }
      }
    }
    keepIt
  }

  def collectFrequentPatterns(node: Noeud, prefix: ArrayBuffer[Int]):Unit={
   for(child <- node.childs){
     if(child.get_frequencyCounter() >= SUPPORT_THRESHOLD){
       val newPrefix:ArrayBuffer[Int] = new ArrayBuffer[Int]()
       newPrefix.addAll(prefix)
       newPrefix.addOne(child.get_value())
       val sizeBefor = patterns.length
       if(child.childs.length > 0){
         collectFrequentPatterns(child,newPrefix)
         if(patterns.size == sizeBefor){
           patterns.addOne(newPrefix.toArray)
         }
       }else
         patterns.addOne(newPrefix.toArray)
     }
   }
  //  patterns
  }
  def computeFrequencyOfValues(domainSizes:Array[Int]):Array[Int]= {
    val frequencies = Array.tabulate(domainSizes.length * (domainSizes.reduceLeft(_ max _) +1))(_ => 0)//)(0)//.fill(domainSizes.length * (domainSizes.reduceLeft(_ max _) + 1))(0)
    for (tuple <- tuples) {
      for (i <- 0 until tuple.size) {
        frequencies(i * combinator.offset + tuple(i)) += 1
      }
    }
    frequencies
  }

  def partition(arr: ArrayBuffer[Int], low:Int, high:Int , frequencyVal: Array[Int]):Int={
    val pivot = frequencyVal(arr(high))
    var i = (low - 1)
    for(j <- low until high){
      if(frequencyVal(arr(j)) >= pivot){
        i+=1
        val temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      }
    }
    val temp = arr(i+1)
    arr(i+1) = arr(high)
    arr(high) = temp
    i+1
  }

  def sort(arr:ArrayBuffer[Int], low:Int, high:Int, frequencyVal: Array[Int]):Unit={
    if(low < high){
      val pi = partition(arr, low, high, frequencyVal)
      sort(arr, low, pi-1, frequencyVal)
      sort(arr, pi+1, high, frequencyVal)
    }
  }



  def partition(FrequentItemsets: ArrayBuffer[Array[Int]], low:Int, high:Int):Int={
    val pivot = FrequentItemsets(high)
    var i = (low - 1)
    for(j <- low until high){
      if(FrequentItemsets(j).length >= pivot.length){
        i+=1
        val temp = FrequentItemsets(i)
        FrequentItemsets(i) = FrequentItemsets(j)
        FrequentItemsets(j) = temp
      }
    }
    val temp = FrequentItemsets(i+1)
    FrequentItemsets(i+1) = FrequentItemsets(high)
    FrequentItemsets(high) = temp
    i+1
  }
  def sort(FrequentItemsets: ArrayBuffer[Array[Int]], low: Int, high:Int): Unit={
    if(low < high){
      val pi = partition(FrequentItemsets, low, high)
      sort(FrequentItemsets, low, pi-1)
      sort(FrequentItemsets, pi+1, high)
    }  }

  def partition2(Entries: ArrayBuffer[entrie], low:Int, high:Int):Int={
    val pivot = Entries(high)
    var i = (low-1)
    for(j <- low until high){
      //if(Entries(j).get_sub_t().get_sub_tup().length*(Entries(j).get_sub_t().get_vars().length+Entries(j).get_pat().get_vars().length) <=pivot.get_sub_t().get_sub_tup().length*(pivot.get_sub_t().get_vars().length+pivot.get_pat().get_vars().length)){

     // if(Entries(j).get_sub_t().get_sub_tup().length<=pivot.get_sub_t().get_sub_tup().length){
       if(Entries(j).get_pat().get_vars().length <=pivot.get_pat().get_vars().length){
        i+=1
        val temp = Entries(i)
        Entries(i) = Entries(j)
        Entries(j) = temp
      }
    }
    val temp = Entries(i+1)
    Entries(i+1) = Entries(high)
    Entries(high) = temp
    i+1
  }
  def sort2(Entries: ArrayBuffer[entrie], low: Int, high:Int): Unit={
    if(low < high){
      val pi = partition2(Entries, low, high)
      sort2(Entries, low, pi-1)
      sort2(Entries, pi+1, high)
    }  }



  def reconstruction (compressed: compressed_table):  Array[List[Int]]={
    val table_or : ArrayBuffer[List[Int]] = new ArrayBuffer[List[Int]]
    for( i <- 0 until  compressed.get_list_entries().length) {
      for(k <- 0 until(compressed.get_list_entries()(i).get_sub_t().get_sub_tup().length)){
        val tuple = new Array[Int](tuples(0).length)
        for (j <- 0 until (compressed.get_list_entries()(i).get_positionVars().length)) {
          if (compressed.get_list_entries()(i).get_positionVars()(j) > 0) {
            val pos = compressed.get_list_entries()(i).get_positionVars()(j) - 1
            tuple(j) = compressed.get_list_entries()(i).get_sub_t().get_sub_tup()(k)(pos)
          }
          else
          {
            val pos = Math.abs(compressed.get_list_entries()(i).get_positionVars()(j) + 1)
            tuple(j) = compressed.get_list_entries()(i).get_pat().get_vals()(pos)
          }
        }
        table_or.addOne(tuple.toList)
      }
    }
    for(i <- 0 until(compressed.get_default_tab().length)) {
      table_or.addOne(compressed.get_default_tab()(i).toList)
    }
    table_or.toArray
  }
}



class FP_Tre() {
  val rootNode : Noeud  = new Noeud(-1)

  def addTransaction( transaction: ArrayBuffer[Int]): Unit={
    var currentParentNode = rootNode
    for (i <- 0 until transaction.length) {
      val existingNode = currentParentNode.getChildWithValue(transaction(i))
      if(existingNode.nonEmpty){
        existingNode.get.set_frequencyCounter()
        currentParentNode = existingNode.get
      }
      else
      {
        val newNode = new Noeud(transaction(i))
        currentParentNode.childs.addOne(newNode)
        currentParentNode = newNode
      }
    }
  }
}

class Noeud(valu: Int) {
  private var value :Int = valu
  private var frequencyCounter = 1
  var childs : ArrayBuffer[Noeud] = new ArrayBuffer[Noeud]()

  def get_frequencyCounter()={
    frequencyCounter
  }
  def set_frequencyCounter():Unit={
    this.frequencyCounter +=1
  }
  def get_value():Int={
    value
  }
  def getChildWithValue(value: Int):Option[Noeud]={
    childs.find(node => node.value == value)
  }
}

