package oscar.cp.compression

import oscar.cp.core.variables.CPIntVar

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.sys.process.ProcessLogger
import sys.process._
class MaximCompress (val tuples: Array[Array[Int]], val variables: Array[CPIntVar]) {
  def MinerFPTree(): compressed_table = {
    //var ordre_tuples =""

    val domains = Array.tabulate(variables.length)(i => variables(i).max)
    var id_ent = 0
    var SUPPORT_THRESHOLD =2
    /*if(Files.exists(Paths.get(tuples.length+"M.txt")))
      {
        import java.io.PrintWriter
        var pw = new PrintWriter(tuples.length+"M.txt")
        pw.close()
        pw = new PrintWriter(tuples.length+"T.txt")
        pw.close()

      }*/
    var taux = 0
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
        taux +=(entry.get_pat().get_vars().length+(entry.get_sub_t().get_sub_tup().length*entry.get_sub_t().get_vars().length))
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
    else{



    //  val vars_motifs :ArrayBuffer[Int] = new ArrayBuffer[Int]()
     // val val_vars_motifs : ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
      var tail_moy = 0
      var freq_moy=0
      val conversion: ArrayBuffer[Int] = new ArrayBuffer[Int]()
      val indVar = Array.tabulate(variables.length)(_ => 0)
      var k = 0
      for (i <- 0 until variables.length) {
        indVar(i) = k
        for (j <- 0 to domains(i)) {
          conversion.addOne(variables(i).min + j)
          k += 1
        }
      }
      val table = tuples.length+"tup.txt"
      val bw = new BufferedWriter(new FileWriter(table))
      for (k <- 0 until tuples.length) {
        var tuple = ""
        var v = 0
        while (v < (variables.length - 1)) {
          tuple += (indVar(v) + (tuples(k)(v) - variables(v).min)).toString + " "
          v+=1
        }
        tuple += (indVar(v) + (tuples(k)(v) - variables(v).min)).toString
        bw.write(tuple + "\n")
      }
      bw.close()
      var  min = (tuples(0).length/2)
      if(min==1)
      {min=2}
      val mn = 0
     //var topK = (tuples.length*0.60).toInt //tuples.length
     val Sm = tuples.length + ".txt"
     val smintop = 2

     var perc = Array(0.50,0.60, 0.7,0.8)
     var sminmoy= 0
     var k0 = 0
   /*
    val pr = Source.fromFile("tp.txt").getLines.mkString.toDouble
       val topK =(tuples.length* pr).toInt//perc(k0)).toInt //tuples.length
   val bww = new BufferedWriter(new FileWriter(Sm))
   bww.write("")
   bww.close()

       "sh scriptTopK.sh " + min + " " + topK +" "+table +" "+smintop+" "+ Sm lineStream_! ProcessLogger(line => ())
   Thread.sleep(5000)
       var line = Source.fromFile(Sm).getLines.mkString
       if (line.equals("") || line.substring(0, 1).equals("-") || line.toInt <= smintop) {
         line = "2" //smintop.toString//
       }*/
 //  sminmoy =  line.toInt
   // sminmoy = (0.2 * tuples.length).toInt//sminmoy+ line.toInt
   //  }
     //sminmoy = sminmoy/4
 /* var sminmoy= 0
  val topK = (tuples.length*0.90).toInt //tuples.length
    "sh scriptTopK.sh " + min + " " + topK +" "+table +" "+smintop+" "+ Sm lineStream_! ProcessLogger(line => ())
    var line = Source.fromFile(Sm).getLines.mkString
    if (line.equals("") || line.substring(0, 1).equals("-") || line.toInt <= smintop) {
      line = "2" //smintop.toString//
    }
    sminmoy =  line.toInt
      println("SMin = "+sminmoy)*/
      SUPPORT_THRESHOLD =10//(0.02 * tuples.length).toInt//sminmoy
     if(SUPPORT_THRESHOLD > 150)
       SUPPORT_THRESHOLD = 100
      // println(min)
      val max = "max.dat"
      "./lcm53 FMfI -l "+min+" "+table+" "+ SUPPORT_THRESHOLD +" "+max lineStream_! ProcessLogger(line => ())
       Thread.sleep(5000)
      val pattern: ArrayBuffer[patInter] = new ArrayBuffer[patInter]()
      for (line <- Source.fromFile(max).getLines()) {
        val sep = " ] \\[ "
        val inter = line.substring(2, line.length).split(sep)

        if(inter(0).length>0){
         pattern.addOne(new patInter(inter(0).split(" ").map(_.toInt), inter(1).split(" ").map(_.toInt).to(ArrayBuffer)))
        }
        }
      print("ici")
      //****************************************Debut du calcul de temps
      //sort(pattern, 0, pattern.length - 1)
      val t1 =  System.currentTimeMillis()
      var list_tuples = ArrayBuffer.tabulate(tuples.length)(i => i)
      val compressd: ArrayBuffer[Int] = new ArrayBuffer[Int]()
      k = 0
      var vide = false
      val vars = Array.tabulate(variables.size)(i => variables(i).name)
      val list_ent: ArrayBuffer[entrie] = new ArrayBuffer[entrie]
      //var id_ent = 0

      var cmp = -1
     val fw = new FileWriter(tuples.length+"M.txt", true)
     print(tuples.length+"M.txt")
     while (k < pattern.length && vide == false && (list_tuples.length >= SUPPORT_THRESHOLD)) {

        bubbleSort(pattern)
        suppChauv(pattern)
        val couv : ArrayBuffer[Int] = new ArrayBuffer[Int]()
        couv.addAll(pattern(0).get_cov())

       //RRETROUVER LES VRAIS MOTIFS (VARIABLE: VALEUR)
      if (couv.length >= SUPPORT_THRESHOLD) {
          cmp+=1
          val vl = Array.tabulate(pattern(0).get_patt().length)(i => conversion(pattern(0).get_patt()(i)))
          val vr: ArrayBuffer[Int] = new ArrayBuffer[Int]
          for (pr <- 0 until (pattern(0).get_patt().length)) {
            var find = false
            var j = indVar.length - 1
            while (j >= 0 && find == false) {
              if (pattern(0).get_patt()(pr) >= indVar(j)) {
                find = true
              }
              else
                j -= 1
            }
            vr.addOne(variables(j).name.substring(1).toInt)
          }
          //ordonner motifs
          for(i<- 1 to vr.length-1){
            for(j <- (i-1) to 0 by -1){
              if(vr(j)>vr(j+1))
              {
                val temp = vr(j + 1)
                val temp1 = vl(j+1)
                vr(j + 1) = vr(j)
                vl(j+1) = vl(j)
                vr(j) = temp
                vl(j)=temp1
              }}}

          val pat: pattern = new pattern(vr.toArray, vl)
          tail_moy +=vl.length
          freq_moy+=couv.length
          val scope_subt: ArrayBuffer[Int] = new ArrayBuffer[Int]()
          for (i <- 0.until(tuples(0).size)) {
            if (!vr.contains(variables(i).name.substring(1).toInt)) {
              scope_subt.addOne(vars(i).substring(1).toInt)
            }
          }
          var tab= ""
          for(vrl<- 0 until(vl.length)){
            val vs = vars.indexOf("V"+vr(vrl)).toString+"="+vl(vrl).toString+" "
            tab +=vs
          }
          tab+=" : "+couv.mkString(" ")
          fw.write(tab+"\n")
          //ordre_tuples += (couv.mkString(" ")+" ")
          val pos = Array.ofDim[Int](pattern(0).get_patt().size)
          val positionsVar = Array.ofDim[Int](tuples(0).size)
          for (i <- 0.to(pat.vr.size - 1)) {
            positionsVar(vars.indexOf("V" + pat.vr(i))) = (-i - 1)
            pos(i) = vars.indexOf("V" + pat.vr(i))
          }
          for (i <- 0.to(scope_subt.size - 1)) {
            positionsVar(vars.indexOf("V" + scope_subt(i))) = (i + 1)
          }
          val sub_tb = Array.tabulate(couv.length, scope_subt.length) { case (i, j) => tuples(couv(i))(vars.indexOf("V" + scope_subt(j))) }
          val sub_table = new sub_tab(scope_subt.toArray, sub_tb, sub_tb.length)
          val entry: entrie = new entrie(pat, sub_table, positionsVar, id_ent)
          id_ent += 1
          list_ent.addOne(entry)
          compressd.addAll(couv)
          list_tuples --= couv
        }
        pattern.remove(0)
        //   sort1(list_ent, 0, list_ent.length - 1)
        if (list_tuples.length < SUPPORT_THRESHOLD)
        {
          vide = true
        }
        k += 1
      }

      sort2(list_ent, 0,  list_ent.length - 1)
      // ordonnerChauv(list_ent)
      fw.close()
      val defaultTable = Array.tabulate(list_tuples.size)(i => tuples(list_tuples(i)))
      val fww = new FileWriter(tuples.length+"T.txt", true) ;
      fww.write(list_tuples.mkString(" "))
      fww.close()
      //Sauvegarder l'ordre des tuples
      //var ordreFInale = list_tuples.mkString(" ")+" "
      //ordreFInale +=  ordre_tuples
      //val fichOrdre = new FileWriter(tuples.length+"Ordre.txt", true)
      //fichOrdre.write(ordreFInale)
      //fichOrdre.close()
      var taille = defaultTable.length * variables.length
      for(ent <- list_ent) {
        taille += ent.get_pat().get_vals().length + (ent.get_sub_t().get_vars().length * ent.get_sub_t().get_sub_tup().length)
      }
      tail_moy = tail_moy/list_ent.length
     freq_moy =freq_moy/list_ent.length
      //val taux =100 -( taille*100/(tuples.length*variables.length))
      val compressed_tab =new compressed_table(list_ent.toArray, defaultTable)
      val fc = new FileWriter(tuples.length+"Comp.txt", true)
      val entete = "Smin;taille;Nb_tp_cmp;taux_cmp;Nbr_mot;tail_mpy_mot;freq_moy"
     // val stat = sminmoy+";"+tuples.length+";"+compressd.size+";"+(100 -((taille*100)/(tuples.length*variables.length))+";"+compressed_tab.get_list_entries().length+";"+tail_moy+";"+freq_moy)
      val stat = 100 -((taille*100)/(tuples.length*variables.length))
      fc.write(stat)
      fc.close()
     println("Taux de compression: "+stat)
      val t2 = System.currentTimeMillis() - t1
   //   print("temps de compression: "+t2)
      compressed_tab
   }
  }






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

  def partition(pattern: ArrayBuffer[patInter], low:Int, high:Int):Int={
    val pivot = pattern(high)
    var i = (low - 1)
    for(j <- low until high){
        if(pattern(j).get_cov().length>= pivot.get_cov().length){
        i+=1
        val temp = pattern(i)
        pattern(i) = pattern(j)
        pattern(j) = temp
      }
    }
    val temp = pattern(i+1)
    pattern(i+1) = pattern(high)
    pattern(high) = temp
    i+1
  }
  def sort(pattern: ArrayBuffer[patInter], low: Int, high:Int): Unit={
    if(low < high){
      val pi = partition(pattern, low, high)
      sort(pattern, low, pi-1)
      sort(pattern, pi+1, high)
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

  def partition2(Entries: ArrayBuffer[entrie], low:Int, high:Int):Int={
    val pivot = Entries(high)
    var i = (low-1)
    for(j <- low until high){
      //if(Entries(j).get_sub_t().get_sub_tup().length*(Entries(j).get_sub_t().get_vars().length+Entries(j).get_pat().get_vars().length)<=pivot.get_sub_t().get_sub_tup().length*(pivot.get_sub_t().get_vars().length+pivot.get_pat().get_vars().length)){
      //if(Entries(j).get_sub_t().get_sub_tup().length>=pivot.get_sub_t().get_sub_tup().length){
      if(Entries(j).get_pat().get_vars().length>=pivot.get_pat().get_vars().length){
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


  //ordonner les motifs dans un ordre decroissant de l'air
  def partitionAir(pattern: ArrayBuffer[patInter], low:Int, high:Int):Int={
    //println(pattern.length)
    val pivot = pattern(high)
    var i = (low - 1)
    for(j <- low until high){
      if(pattern(j).get_cov().length>= pivot.get_cov().length){
        //if(pattern(j).get_patt().length>= pivot.get_patt().length){
        //if(pattern(j).get_cov().length*pattern(j).get_patt().length>= pivot.get_cov().length*(pivot.get_patt().length)){
        i+=1
        val temp = pattern(i)
        pattern(i) = pattern(j)
        pattern(j) = temp
      }
    }
    val temp = pattern(i+1)
    pattern(i+1) = pattern(high)
    pattern(high) = temp
    i+1
  }
  def sortAir(pattern: ArrayBuffer[patInter], low: Int, high:Int): Unit={
    if(low < high){
      val pi = partitionAir(pattern, low, high)
      sortAir(pattern, low, pi-1)
      sortAir(pattern, pi+1, high)
    }  }

  //SUpprimer la liste des motifs qui se chevauchent avec le motif actuel
  def suppChauv(pattern: ArrayBuffer[patInter]): Unit ={
    val patt: ArrayBuffer[patInter] = new ArrayBuffer[patInter]()
    for(p <- 1 until pattern.length){
      if((pattern(0).get_cov().toSet.intersect(pattern(p).get_cov().toSet)).size> 0){
        patt.addOne(pattern(p))
      }
    }
    pattern --= patt
  }
  // ordonner les motifs selon leur degre de chevauchement:
  def ordonnerChauv(Entries: ArrayBuffer[entrie]): Unit={
    for(i <- 0 until  Entries.length){
      var taux = 0.0
      for(j <- 0 until(Entries.length)){
        if(i != j){
          taux +=Entries(i).get_pat().get_vars().toSet.intersect(Entries(j).get_pat().get_vars().toSet).size/Entries(i).get_pat().get_vars().toSet.union(Entries(j).get_pat().get_vars().toSet).size
        }
      }
      Entries(i).set_chauv(taux/Entries.length)
    }
    // < chevauchement decroissant
    // > chauvauchement croissant
    for(i<- 1 to Entries.length-1) {
      for (j <- (i - 1) to 0 by -1) {
        if(Entries(j).get_chauv()>Entries(j+1).get_chauv()) {
          val temp = Entries(j + 1)
          Entries(j + 1) = Entries(j)
          Entries(j) = temp
        }
      }
    }
  }



  //TRI PAR TAS
  def sortTriTas(a: ArrayBuffer[patInter]): Unit = {
    var m = a.length - 1
    buildHeap(a, m)
    while (m >= 1) {
      swap(a, 0, m)
      m-=1
      heapify(a, 0, m)
    }
  }

  def buildHeap(a: ArrayBuffer[patInter], m: Int): Unit = {
    for (i <- m/2 to 0 by -1) {
      heapify(a, i, m)
    }
  }

  def heapify(a: ArrayBuffer[patInter], loc: Int, lastLeaf: Int): Unit = {
    val l = left(loc)
    val r = right(loc)

    var max = loc
    val al = a(l).get_cov().length*a(l).get_patt().length
    val amax = a(max).get_cov().length*a(max).get_patt().length
    val ar = a(r).get_cov().length*a(r).get_patt().length
    if(l <= lastLeaf && al > amax) max = l
    if(r <= lastLeaf && ar > amax) max = r

    if(max != loc) {
      swap(a, max, loc)
      heapify(a, max, lastLeaf)
    }
  }

  def right(loc: Int): Int = {
    return 2*loc+1
  }
  def left(loc: Int): Int = {
    return 2*loc
  }
  def swap(pattern: ArrayBuffer[patInter], i: Int, j:Int): Unit = {
    val staging = pattern(i)
    pattern(i) = pattern(j)
    pattern(j) = staging
  }


  def bubbleSort(a:ArrayBuffer[patInter]):ArrayBuffer[patInter]={
    for(i<- 1 to a.length-1){
      for(j <- (i-1) to 0 by -1){
        //  if((a(j).get_patt().length/a(j).get_cov().length)<(a(j+1).get_patt().length/a(j+1).get_cov().length)){
        // if(a(j).get_cov().length*a(j).get_patt().length<a(j+1).get_cov().length*a(j+1).get_patt().length ){
        // if((a(j).get_patt().length > a(j+1).get_patt().length)&&( (tuples.length*0.08).toInt > a(j).get_cov().length )&& (a(j).get_cov().length > (tuples.length*0.0005).toInt)) {
        if(a(j).get_patt().length*a(j).get_cov().length < a(j+1).get_patt().length*a(j+1).get_cov().length){
          // val cmpj = (a(j).get_patt().length+(a(j).get_cov().length*(tuples(0).length-a(j).get_patt().length))+((tuples.length - a(j).get_cov().length)*tuples(0).length))
          //val cmpj1 = (a(j+1).get_patt().length+(a(j+1).get_cov().length*(tuples(0).length-a(j+1).get_patt().length))+((tuples.length - a(j+1).get_cov().length)*tuples(0).length))
          //if(cmpj > cmpj1){
          val temp = a(j + 1)
          a(j + 1) = a(j)
          a(j) = temp
        }
      }
    }
    a
  }
}




class patInter(val pat: Array[Int], val cov: ArrayBuffer[Int]){
  private var patt = pat
  private var cove = cov
  def set_patt(x: Array[Int]){
    patt = x
  }
  def get_patt(): Array[Int] ={
    patt
  }
  def set_cov(x: ArrayBuffer[Int]){
    cove.clear()
    cove.addAll(x)
  }
  def get_cov(): ArrayBuffer[Int] ={
    cove
  }


}