def recurrentSeries[ T ]( prev: T )( next: T => T ): LazyList[ T ] =
  prev #:: recurrentSeries(next(prev))(next)
  