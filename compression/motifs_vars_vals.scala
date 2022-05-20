package oscar.cp.compression

import scala.collection.mutable.ArrayBuffer

class motifs_vars_vals ( ){
  private val liste_vars : ArrayBuffer[Int] = new ArrayBuffer[Int]
  private val liste_pat_vars :ArrayBuffer[ArrayBuffer[Int]]= new ArrayBuffer[ArrayBuffer[Int]]
  //private val liste_motifs_vars_vals : ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]] = new ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]

  def get_liste_vars():ArrayBuffer[Int]={
    liste_vars
  }

  def set_liste_vars(l: ArrayBuffer[Int])={
    liste_vars.addAll(l)
  }

  def get_liste_pat_vars():ArrayBuffer[ArrayBuffer[Int]]={
    liste_pat_vars
  }
  def set_liste_pat_vars(l:ArrayBuffer[ArrayBuffer[Int]])={
    liste_pat_vars.addAll(l)
  }
  /*def get_liste_motifs_vars_vals():ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]={
    liste_motifs_vars_vals
  }
  def set_liste_motifs_vars_vals(l:ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]])={
    liste_motifs_vars_vals.addAll(l)
  }*/


}
