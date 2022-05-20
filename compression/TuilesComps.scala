package oscar.cp.compression

import oscar.cp.core.variables.CPIntVar

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import sys.process._
class TuilesComps (val tuples: Array[Array[Int]], val variables: Array[CPIntVar]){
  def MinerFPTree(): compressed_table = {
    val domains = Array.tabulate(variables.length)(i => variables(i).max)
    var id_ent = 0
    var taux = 0
    if(Files.exists(Paths.get(tuples.length+"M.txt")))
    {
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
      compressed_tab
    }
    else
      {
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
        //Table convertie
        val table = tuples.length+"tup.dat"
        val table_convertie: ArrayBuffer[Array[String]] = new ArrayBuffer[Array[String]]()
        //val table1 = tuples.length+"tup1.txt"
        val bw = new BufferedWriter(new FileWriter(table))
        //val bw1 = new BufferedWriter(new FileWriter(table1))
        for (k <- 0 until tuples.length) {
          var tuple = ""
          var v = 0
          while (v < (variables.length - 1)) {
            tuple += (indVar(v) + (tuples(k)(v) - variables(v).min)).toString + " "
            v+=1
          }
          tuple += (indVar(v) + (tuples(k)(v) - variables(v).min)).toString
          bw.write(tuple + "\n")
          table_convertie.addOne(tuple.split(" "))
          //bw1.write(tuple + "\n")
        }
        bw.close()
        //bw1.close()
        //AJOUTER LA PARTIE SUR LES TUILES
        //python script.py 2 example-wine.dat  data.txt
        val nbr_tuiles = 10

        val nom_fich_tuiles = tuples.length.toString+"_tiling.txt"
        //val path = Paths.get(nom_fich_tuiles)
        //Files.createFile(path)
        "python replace.py "+table  lineStream_! ProcessLogger(line => ())
        "python script.py " + nbr_tuiles + " " + table +" "+nom_fich_tuiles lineStream_! ProcessLogger(line => ())
        //"python script.py " + nbr_tuiles + " " + table +" "+nom_fich_tuiles lineStream_! ProcessLogger(line => ())


        var list_tuples = ArrayBuffer.tabulate(tuples.length)(i => i)
      //calcul des couvertures
        val pattern: ArrayBuffer[patInter] = new ArrayBuffer[patInter]()
        for (line <- Source.fromFile(nom_fich_tuiles).getLines()) {
          val couvv : ArrayBuffer[Int] = new ArrayBuffer[Int]()
          for(i <- 0 until(list_tuples.length)){
            if(contains_str(line.split(" ").to(ArrayBuffer), table_convertie(list_tuples(i)))){
              couvv.addOne(list_tuples(i))
            }
          }
          pattern.addOne(new patInter(line.split(" ").map(_.toInt), couvv))
          list_tuples --= couvv
        }
        //Generer les vrais motifs
        //var list_tuples = ArrayBuffer.tabulate(tuples.length)(i => i)
        val compressd: ArrayBuffer[Int] = new ArrayBuffer[Int]()
        val vars = Array.tabulate(variables.size)(i => variables(i).name)
        val list_ent: ArrayBuffer[entrie] = new ArrayBuffer[entrie]
        val fw = new FileWriter(tuples.length+"M.txt", true)
        var cmp = -1
        for( k <- 0 until(pattern.length)){
          val couv : ArrayBuffer[Int] = new ArrayBuffer[Int]()
          couv.addAll(pattern(0).get_cov())
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
          //list_tuples --= couv
        }
        val defaultTable = Array.tabulate(list_tuples.size)(i => tuples(list_tuples(i)))
        val fww = new FileWriter(tuples.length+"T.txt", true) ;
        fww.write(list_tuples.mkString(" "))
        fww.close()
        var taille = defaultTable.length * variables.length
        for(ent <- list_ent) {
          taille += ent.get_pat().get_vals().length + (ent.get_sub_t().get_vars().length * ent.get_sub_t().get_sub_tup().length)
        }
        tail_moy = tail_moy/list_ent.length
        val compressed_tab =new compressed_table(list_ent.toArray, defaultTable)
        val fc = new FileWriter(tuples.length+"Comp.txt", true)
        val stat = "taille: "+tuples.length+"\n"+"Nb_tp_cmp "+compressd.size+"\nNb_tp_cmp "+defaultTable.size+"\ntaux_cmp: "+(100 -((taille*100)/(tuples.length*variables.length))+"\nNbr_mot: "+compressed_tab.get_list_entries().length+"\ntail_mpy_mot: "+tail_moy+"\nfreq_moy: "+freq_moy)
        fc.write(stat)
        fc.close()
        compressed_tab
      }
  }
def contains_str(list_it:ArrayBuffer[String], tuples: Array[String]): Boolean={
  var exist : Boolean = true
  var i = 0
  while (exist && i < list_it.length){
    if (tuples.contains(list_it(i))){
      i+=1
    }else
      exist = false
  }
  exist
}

}
