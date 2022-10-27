import java.awt.{ GridBagConstraints, Insets }

package object CG {
  //ряды
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  //
  def getConstraints(
                      gridx:Int,
                      gridy:Int,
                      gridheight:Int,
                      gridwidth:Int,
                      gridweightx:Int = 0,
                      gridweighty:Int = 0,
                      anchor:Int = 10,
                      fill:Int = 0,
                      insets:Insets = new Insets(0,0,0,0),
                      ipadx:Int = 0,
                      ipady:Int = 0,
                    ): GridBagConstraints = {
    new GridBagConstraints(gridx,gridy,gridwidth,gridheight,gridweightx,gridweighty,anchor,fill,insets,ipadx,ipady)
  }
  
}
