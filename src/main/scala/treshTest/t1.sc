
Set(1,2,3,4,10,5,6) -- Set(1,2,3,4,11,5,6)

def recurrentSeries[ T ]( prev: T )( next: T => T ): LazyList[ T ] =
  prev #:: recurrentSeries(next(prev))(next)
  
def x2 = recurrentSeries(2)(_*2)
x2.take(10).force
println(x2)