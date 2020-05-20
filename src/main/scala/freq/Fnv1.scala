package freq

object Fnv1 {
  val H: Int = 0x811c9dc5
  val P: Int = 0x01000193

  def next(hash: Int, value: Byte): Int  = (hash ^ value) * P
}
