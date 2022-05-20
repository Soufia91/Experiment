package oscar.cp.compression
import oscar.cp.core.variables.CPIntVar

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.collection.mutable.ArrayBuffer
import sys.process._
import java.nio.file.{Paths, Files}
class FP_Tree( val tuples: Array[Array[Int]],  val variables: Array[CPIntVar]) {
  val domains = Array.tabulate(tuples(0).length)(_ => 0)
  for(t <- 0 until tuples.length) {
    for {v <- 0 until tuples(0).length}{ if(domains(v) < tuples(t)(v)) { domains(v) = tuples(t)(v)}}
  }
  var SUPPORT_THRESHOLD =2
  var combinator: CombinatorOfTwoInts = new CombinatorOfTwoInts(domains.length, domains.reduceLeft(_ max _), 0, 0, 0, 0)
  val FrequentItemsets: ArrayBuffer[ArrayBuffer[Node]] = new ArrayBuffer[ArrayBuffer[Node]]
  def MinerFPTree(): compressed_table = {
    val t1 =  System.currentTimeMillis()
   // for(v <- 0 until(variables.length)){print(variables(v).name+" ")}
    var id_ent = 0
    //println(Files.exists(Paths.get(tuples.length+"M.txt")))
   if(Files.exists(Paths.get(tuples.length+"M.txt")))
    { val vars = Array.tabulate(variables.size)(i => variables(i).name)
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
        list_ent.addOne(entry)
      }
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
      println("table_or: "+tuples.length+"  table_rec: "+table_ori.length+" sous: "+list_tupl.length)*/
      compressed_tab
    }else
    {






      val table ="table.txt"
      val file = new File(table)
      val bw = new BufferedWriter(new FileWriter(file))
      for (i <- 0 until tuples.length) {
        var tuple = ""
        var j = 0
        for (j <- 0 until (tuples(0).length - 1)) {
          tuple += (j + 1).toString + (tuples(i)(j) - variables(j).min + 1).toString + " "
        }
        j += 1
        tuple += (j + 1).toString + (tuples(i)(j) - variables(j).min + 1).toString
        bw.write(tuple + "\n")
      }

      bw.close()
      var  min = 2//(tuples(0).length/2)-1
      if(min==1)
      {min=2}
    val topK = tuples.length//(tuples.length*0.6).toInt

      val Sm = tuples.length + ".txt"
      val smintop = 5
      "sh scriptTopK.sh " + min + " " + topK +" "+table +" "+smintop+" "+ Sm lineStream_! ProcessLogger(line => ())
      var line = Source.fromFile(Sm).getLines.mkString
      if (line.equals("") || line.substring(0, 1).equals("-") || line.toInt <= smintop) {
        line =smintop.toString// "10"
      }else
        if(line.toInt > 100)
          {
            line =(80).toString
          }
    if(line.toInt < 20)
    {
      line =(4).toString
    }
      SUPPORT_THRESHOLD = line.toInt
      var tail_moy = 0
      var freq_moy=0
      val valueFrequencies = computeFrequencyOfValues(domains)
      val list_ent: ArrayBuffer[entrie] = new ArrayBuffer[entrie]
      val tree: Tree = new Tree()
      var j = 0
      for (tuple <- tuples) {
        val transaction: ArrayBuffer[Int] = new ArrayBuffer[Int]
        for (i <- 0.to(tuple.length - 1)) {
          val value = combinator.combineIntValueFor(i, tuple(i))
          if (valueFrequencies(value) >= SUPPORT_THRESHOLD)
            transaction.addOne(value)
        }
        sort(transaction, 0, transaction.length - 1, valueFrequencies)
        tree.addTransaction(transaction, j)
        j += 1
      }
      if (tree.rootNode.children.size > 0) {
        for (child <- tree.rootNode.children) {
          val prefixList: ArrayBuffer[Node] = new ArrayBuffer[Node]
          prefixList.addOne(child)
          if (child.cover.size >= SUPPORT_THRESHOLD) {
            for (child1 <- child.children) {
              val prefixList1: ArrayBuffer[Node] = new ArrayBuffer[Node]()
              prefixList1.addAll(prefixList)
              prefixList1.addOne(child1)
              freqNode(child1,prefixList1, 2, tuples(0).length)
            }
          }
        }
      }
      //Itemsets
      val list_tuples = ArrayBuffer.tabulate(tuples.size)(i => i)
      val tuplesComp: ArrayBuffer[Int] = new ArrayBuffer[Int]()
      //var id_ent = 0
      var fill = ""
      for(v <- variables) {
        fill+= v.name
      }
      val vars = Array.tabulate(variables.size)(i => variables(i).name)
      val fw = new FileWriter(tuples.length+"M.txt", true) ;
      var prcc = 0
     // println("Nombre d'itemsets: "+FrequentItemsets.length)
      while(prcc< FrequentItemsets.length && tuplesComp.length<= (list_tuples.length-SUPPORT_THRESHOLD))
      {

        val itemset = FrequentItemsets(prcc)
        val vr = Array.tabulate(itemset.size)(k => variables(itemset(k).value / (domains.reduceLeft(_ max _) +1)).name.substring(1).toInt)
        val vl = Array.tabulate(itemset.size)(k => itemset(k).value % (domains.reduceLeft(_ max _) + 1))
        sortp(vr,vl, 0, vr.length-1)
        val pat: pattern = new pattern(vr, vl)
        val couverture = itemset(itemset.length-1).covr
        var tab= ""
        for(vrl<- 0 until(vl.length)){
          val vs = vars.indexOf("V"+vr(vrl)).toString+"="+vl(vrl).toString+" "
          tab +=vs
        }
      //  println(tab)
        tab+=" : "+couverture.mkString(" ")
        fw.write(tab+"\n")
        tuplesComp.addAll(couverture)
        tail_moy+=vl.length
        freq_moy+=couverture.length
        val scope_subt: ArrayBuffer[Int] = new ArrayBuffer[Int]()
        for (i <- 0.until(tuples(0).size)) {
          if (!vr.contains(variables(i).name.substring(1).toInt)) {
            scope_subt.addOne(vars(i).substring(1).toInt)
          }
        }
        val sub_tb = Array.tabulate(couverture.length, scope_subt.length) { case (i, j) => tuples(couverture(i))(vars.indexOf("V" + scope_subt(j))) }
        val sub_table = new sub_tab(scope_subt.toArray, sub_tb, sub_tb.length)
        val positionsVar = Array.ofDim[Int](tuples(0).size)
        for (i <- 0.to(pat.vr.size - 1)) {
          positionsVar(vars.indexOf("V" + pat.vr(i))) = (-i - 1)
        }
        for (i <- 0.to(scope_subt.size - 1)) {
          positionsVar(vars.indexOf("V" + scope_subt(i))) = (i + 1)
        }
        val entry: entrie = new entrie(pat, sub_table, positionsVar, id_ent)
        id_ent += 1
        list_ent.addOne(entry)
        prcc+=1

      }
      fw.close()
      //  sort2(list_ent, 0,  list_ent.length-1)
      val defaultTable: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]]()
      //if(tuplesComp.length>0){
        list_tuples --= tuplesComp
       // println(list_tuples)
        defaultTable.addAll(Array.tabulate(list_tuples.size)(i => tuples(list_tuples(i))))
       val fw1 = new FileWriter(tuples.length+"T.txt", true) ;
    fw1.write(list_tuples.mkString(" "))
      fw1.close()
      //}
      //println("Def table: "+defaultTable.length)
      val compressed_tab =new compressed_table(list_ent.toArray, defaultTable.toArray)
      var taille = 0
      for(m <- list_ent){
        taille += m.get_sub_t().get_sub_tup().length * m.get_sub_t().get_vars().length
      }
      taille += defaultTable.length * variables.length
      if(list_ent.length>0)
        {
          tail_moy = tail_moy/list_ent.length
          freq_moy=freq_moy/list_ent.length
        }


   //   val fc = new FileWriter("Comp.txt", true)
    val entete = "Smin;taille;Nb_tp_cmp;taux_cmp;Nbr_mot;tail_mpy_mot;freq_moy"
    val stat = SUPPORT_THRESHOLD+";"+tuples.length+";"+tuplesComp.size+";"+(100 -((taille*100)/(tuples.length*variables.length))+";"+compressed_tab.get_list_entries().length+";"+tail_moy+";"+freq_moy+"\n")
    //fc.write(stat)
    //fc.close()
  print(stat)
      val table_ori= reconstruction(compressed_tab)
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
      //println("table_or: "+tuples.length+"  table_rec: "+table_ori.length+" sous: "+list_tupl.length)
      val t2 = System.currentTimeMillis() - t1
    //("\n temps de compression: "+t2)
      compressed_tab
   }
  }

  def printTree(node: Node, prefix:String):Unit={
    if(node.children.nonEmpty){
      for(child <- node.children)
        printTree(child, prefix+" "+child.value+" ("+child.cover.size+")"+"\n")
    }
    else
      printf(prefix+"\n")
  }

  def computeFrequencyOfValues(domainSizes:Array[Int]):Array[Int]= {
    //val frequencies = Array.fill(domainSizes.length * (domainSizes.reduceLeft(_ max _) + 1))(0)

    val frequencies = Array.tabulate(domainSizes.length * (domainSizes.reduceLeft(_ max _) +1))(_ => 0)//)(0)//.fill(domainSizes.length * (domainSizes.reduceLeft(_ max _) + 1))(0)
    for (tuple <- tuples) {
      for (i <- 0 until tuple.size) {
        frequencies(i * combinator.offset + tuple(i)) += 1
      }
    }
    frequencies
  }
  /*def frequentPatterns(tree: Tree, arite:Int):Unit={
    var siz =1

    val patterns : ArrayBuffer[ArrayBuffer[Node]] =  ArrayBuffer[ArrayBuffer[Node]](null)
    val successor = tree.rootNode.children
    if(successor.length > 0){
      for(child <- successor){
        if(child.cover.length >= 2){
          val prefixList1 : ArrayBuffer[Node] =  new ArrayBuffer[Node]
          prefixList1.addOne(child)
          val successor1 = child.children
          for(child1 <- successor1){
            val prefixList2 =prefixList1
            prefixList2.addOne(child1)
            println("pref: "+prefixList2.length)
            freqNode(child1, patterns,prefixList2,2,arite)
          }
        }
      }
    }

  */
  def freqNode(node:Node,  prefixList: ArrayBuffer[Node], siz:Int, arite:Int): Unit = {
    if(node.children.length>0){
      val list_ch: ArrayBuffer[Node] = new ArrayBuffer[Node]
      var som_f : Int = 0
      for(child <- node.children){
        if(child.cover.length >= SUPPORT_THRESHOLD){
          som_f+= child.cover.length
          list_ch.addOne(child)
        }
      }

      if(list_ch.length>0){
        val gain = node.cover.length *(arite - siz) + siz
        var gain_f = list_ch.length * (siz + 1) + (som_f * (arite - (siz+1)))
        if(list_ch.size < node.children.length){
          val dif = node.cover.size - som_f
          if(dif>=SUPPORT_THRESHOLD && siz>2){
            gain_f += (siz + (dif * (arite - siz)))
            if(gain_f <= gain) {
              if(prefixList.length>2){
                val pref: ArrayBuffer[Node] = new ArrayBuffer[Node]
                pref.addAll(prefixList)
                val cov: ArrayBuffer[Int] = new ArrayBuffer[Int]
                for (nd <- node.children) {
                  if (nd.cover.size < SUPPORT_THRESHOLD) {
                    cov.addAll(nd.cover)
                  }
                }
                pref(pref.size - 1).cover.clear()
                pref(pref.size - 1).cover.addAll(cov)
                //println(pref(pref.size-1).cover.length)
                FrequentItemsets.addOne(pref)
              }
              for(child <- list_ch){
                val prefixList1: ArrayBuffer[Node] = new ArrayBuffer[Node]
                prefixList1.addAll(prefixList)
                prefixList1.addOne(child)
                freqNode(child, prefixList1, siz+1,arite)
              }
            }
            else {
              FrequentItemsets.addOne(prefixList)
            }
          }
          else{
            gain_f += (arite * dif)
            if(gain_f <= gain){
              for(child <- list_ch){
                val prefixList1: ArrayBuffer[Node] = new ArrayBuffer[Node]
                prefixList1.addAll(prefixList)
                prefixList1.addOne(child)
                freqNode(child, prefixList1, siz+1,arite)
              }
            }
            else {
              FrequentItemsets.addOne(prefixList)
            }
          }
        }
        else{
          if(gain_f <= gain){
            for (child <- list_ch) {
              val prefixList1: ArrayBuffer[Node] = new ArrayBuffer[Node]
              prefixList1.addAll(prefixList)
              prefixList1.addOne(child)
              freqNode(child,  prefixList1, siz + 1, arite)
            }
          }
          else {
            FrequentItemsets.addOne(prefixList)
          }
        }
      }
      else{
        if(prefixList(prefixList.size-1).cover.size>=SUPPORT_THRESHOLD && prefixList.length>=2)
        {

          //  println("add")
          FrequentItemsets.addOne(prefixList)
        }
      }
    }
    else{
      if((prefixList(prefixList.size-1).cover.length>=SUPPORT_THRESHOLD) && prefixList.length>=2){
        FrequentItemsets.addOne(prefixList)
      }
    }
    //return patterns
  }
  def partition(arr: ArrayBuffer[Int], low:Int, high:Int , frequencyVal: Array[Int]):Int={
    val pivot = frequencyVal(arr(high))
    var i = (low - 1)
    for(j <- low until high){
      if(frequencyVal(arr(j)) <= pivot){
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

  def partitionp(vr: Array[Int], vl: Array[Int], low:Int, high:Int ):Int={
    val pivot = vr(high)
    var i = (low - 1)
    for(j <- low until high){
      if(vr(j) <= pivot){
        i+=1
        val temp = vr(i)
        vr(i) = vr(j)
        vr(j) = temp
        val temp1 = vl(i)
        vl(i) = vl(j)
        vl(j) = temp1
      }
    }
    val temp = vr(i+1)
    vr(i+1) = vr(high)
    vr(high) = temp

    val temp1 = vl(i+1)
    vl(i+1) = vl(high)
    vl(high) = temp1
    i+1
  }

  def sortp(vr:Array[Int],vl:Array[Int], low:Int, high:Int):Unit={
    if(low < high){
      val pi = partitionp(vr, vl,low, high )
      sortp(vr,vl, low, pi-1)
      sortp(vr, vl,pi+1, high)
    }
  }



  def partition(FrequentItemsets: ArrayBuffer[ArrayBuffer[Node]], low:Int, high:Int):Int={
    val pivot = FrequentItemsets(high)
    var i = (low - 1)
    for(j <- low until high){
      if(FrequentItemsets(j).length <= pivot.length){
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
  def sort(FrequentItemsets: ArrayBuffer[ArrayBuffer[Node]], low: Int, high:Int): Unit={
    if(low < high){
      val pi = partition(FrequentItemsets, low, high)
      sort(FrequentItemsets, low, pi-1)
      sort(FrequentItemsets, pi+1, high)
    }  }


  def partition1(Entries: ArrayBuffer[entrie], low:Int, high:Int):Int={
    val pivot = Entries(high)
    var i = (low - 1)
    for(j <- low until high){
      if(Entries(j).get_sub_t().get_sub_tup().length*(Entries(j).get_sub_t().get_vars().length+Entries(j).get_pat().get_vars().length) >=pivot.get_sub_t().get_sub_tup().length*(pivot.get_sub_t().get_vars().length+pivot.get_pat().get_vars().length)){
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
  def sort1(Entries: ArrayBuffer[entrie], low: Int, high:Int): Unit={
    if(low < high){
      val pi = partition1(Entries, low, high)
      sort1(Entries, low, pi-1)
      sort1(Entries, pi+1, high)
    }  }


  def partition2(Entries: ArrayBuffer[entrie], low:Int, high:Int):Int={
    val pivot = Entries(high)
    var i = (low-1)
    for(j <- low until high){
      //if(Entries(j).get_sub_t().get_sub_tup().length*(Entries(j).get_sub_t().get_vars().length+Entries(j).get_pat().get_vars().length) <=pivot.get_sub_t().get_sub_tup().length*(pivot.get_sub_t().get_vars().length+pivot.get_pat().get_vars().length)){

      if(Entries(j).get_sub_t().get_sub_tup().length>=pivot.get_sub_t().get_sub_tup().length){
        // if(Entries(j).get_pat().get_vars().length >=pivot.get_pat().get_vars().length){
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
    for(i <- 0 until(compressed.get_default_tab().length))
      table_or.addOne(compressed.get_default_tab()(i).toList)
    // table_or ++= compressed.get_default_tab()
    table_or.toArray
  }

}


class Tree() {
  val rootNode = new Node(-1, null, null)

  def addTransaction( transaction: ArrayBuffer[Int], id_trans: Int): Unit={
    var currentParentNode = rootNode
    for (i <- 0 until transaction.length) {
      // If path exists, increment, if not, create new node
      val existingNode = currentParentNode.getChildWithValue(transaction(i))
      if(existingNode.nonEmpty){
        existingNode.get.cover.addOne(id_trans)
        currentParentNode = existingNode.get
      }
      else
      {
        val cover: ArrayBuffer[Int] = new ArrayBuffer[Int]
        cover.addOne(id_trans)
        val newNode = new Node(transaction(i), cover, currentParentNode)
        currentParentNode.children.addOne(newNode)
        currentParentNode = newNode
      }
    }
  }
}




class Node(val value: Int, var covr: ArrayBuffer[Int], val parent: Node) {
  var cover = new ArrayBuffer[Int]()
  this.cover = covr
  var children = new ArrayBuffer[Node]()
  def getChildWithValue(value: Int): Option[Node] = {
    children.find(node => node.value == value)
  }
}

