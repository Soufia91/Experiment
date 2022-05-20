package oscar.cp.compression

class compressed_table (val list_ent:Array[entrie], val dflt_tab: Array[Array[Int]]){
  private var list_entries = list_ent
  private var default_tab = dflt_tab

  def set_list_entries(x: Array[entrie]){
    list_entries= x
  }
  def get_list_entries(): Array[entrie] ={
    list_entries
  }

  def set_default_tab(x: Array[Array[Int]]){
    default_tab= x
  }
  def get_default_tab(): Array[Array[Int]] ={
    default_tab
  }

}
