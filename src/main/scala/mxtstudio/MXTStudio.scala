/**
 * MixtapeStudio
 * 
 * A Scala DSL
 *
 * CS345, Assignment 5, Spring 2016
 *
 * Alex Gerome, Eric Chao
 *
 *
 */
package mxtstudio {

    import scala.collection.mutable

    class MXTStudio{
      abstract sealed class MXTLine

      /**
       * Bindings holds the two types of values provided, atoms and numerics.
     	 * It takes a type parameter on initialization corresponding to the
     	 * actual type.
     	 */
      class Bindings[T,U] {
        val atoms = mutable.HashMap[Symbol, T]()
        val numerics = mutable.HashMap[Symbol, U]()

        /**
      	 * set uses a little hack to allow the storage of either one type or
    	   * another, but none other.
    	   */
    	  def set[X >: T with U](k:Symbol, v:X) = v match {
          case u:U => numerics(k) = u
          case t:T => atoms(k) = t
       	}
      	
        def atom(k:Symbol):T = atoms(k)
        def num(k:Symbol):U = numerics(k)
      }
      
      //The current line?
      //var current: Int = 1
      
      val lines = new mutable.HashMap[Int, MXTLine]
      val binds = new Bindings[String, Int]
      val returnStack = new mutable.Stack[Int]
          
      //Implement assignments
      /**
    	* The Assignment class is used by the `symbol2Assignment` implicit to
    	* stand-in for a Scala symbol in the LET form.  This class returns
     	* a function of () => Unit that does the appropriate binding.
     	*/
      case class Assignment(sym:Symbol) {
        def :=(v:String):() => Unit = () => binds.set(sym, v)
        def :=(v:Int):() => Unit = () => binds.set(sym, v)
        def :=(v:() => Int):() => Unit = () => binds.set(sym, v())
        
        /*
         *  Might need a custom assignment which tracks lines
         */
        /* Example:
         * def chord(v: String): Unit = {
         * 	 lines(current) = Assign(current, (() => binds.set(sym, v)))
         *   current++
         * }
         */
      }
    }
}
