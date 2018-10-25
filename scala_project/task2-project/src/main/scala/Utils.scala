
object Util {
    def toOption[T](inst: T): Option[T] = {
        if(inst != null)
            return Some(inst)
        return None
    }
}