

def recurrentSeries[ T ]( prev: T )( next: T => T ): LazyList[ T ] =
  prev #:: recurrentSeries(next(prev))(next)


def wikiTable2Transposition(table:String) =
    table
      .replace('	',' ')
      .replace('\n',' ')
      .split(' ')
      .collect{ case str:String if str.nonEmpty => str.toInt - 1}
      .toSeq


"32\t1\t2\t3\t4\t5\n4\t5\t6\t7\t8\t9\n8\t9\t10\t11\t12\t13\n12\t13\t14\t15\t16\t17\n16\t17\t18\t19\t20\t21\n20\t21\t22\t23\t24\t25\n24\t25\t26\t27\t28\t29\n28\t29\t30\t31\t32\t1".split("(\t|\n)")
