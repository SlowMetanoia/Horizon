import Educational.Sugar.demo


//классический if
def IF(condition: =>Boolean)(code: =>Unit): Unit ={
  condition&&{code ;true}
}
IF(true){ println("1")}
IF(false){ println("0")}

//while
def WHILE(condition: =>Boolean)(code: =>Unit):Unit = {
  condition&&{code; WHILE(condition)(code); true}
}

var i = 10
WHILE(i>0){
  println(i)
  i-=1
}

demo.main(Array())






val c = 1 + 2




val c = (1 to 10) map { i => i * i }












