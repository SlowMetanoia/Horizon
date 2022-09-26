
Set(1,2,3,4,10,5,6) -- Set(1,2,3,4,11,5,6)

def recurrentSeries[ T ]( prev: T )( next: T => T ): LazyList[ T ] =
  prev #:: recurrentSeries(next(prev))(next)
  
def x2 = recurrentSeries(2)(_*2)
x2.take(10).force
println(x2)

val ch:Char = 'a'

((ch+2*Int.MaxValue).toChar - 2*Int.MaxValue).toChar

def transpositionArrFromWiki =
  """
    |58	50	42	34	26	18	10	2	60	52	44	36	28	20	12	4
    |62	54	46	38	30	22	14	6	64	56	48	40	32	24	16	8
    |57	49	41	33	25	17	9	1	59	51	43	35	27	19	11	3
    |61	53	45	37	29	21	13	5	63	55	47	39	31	23	15	7
    |""".stripMargin.replace('	',' ').replace('\n',' ').split(' ').collect{
    case str:String if str.nonEmpty => str.toInt - 1
  }.toSeq

transpositionArrFromWiki

Seq(57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3, 61, 53, 45, 37, 29,
    21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7, 56, 48, 40, 32, 24, 16, 8, 0, 58, 50,
    42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4, 62, 54, 46, 38, 30, 22,
    14, 6).zipWithIndex
          .sortWith(_._1 > _._1)
          .map(_._2)


