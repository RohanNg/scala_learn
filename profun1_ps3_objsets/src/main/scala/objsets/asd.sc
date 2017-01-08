import objsets._

val x1 = new Tweet("a","a",1)
val x2 = new Tweet("a","b",4)
val x3 = new Tweet("a","c",7)
val x4 = new Tweet("a","d",3)
val x5 = new Tweet("a","e",2)

val x6 = new Tweet("b","f",9)
val x7 = new Tweet("b","g",1)
val x8 = new Tweet("b","h",23)
val x9 = new Tweet("b","i",15)
val x10 = new Tweet("b","j",6)

val s = new NonEmpty(x1, new Empty, new Empty)

val a = s.incl(x1).incl(x2).incl(x3).incl(x4).incl(x5)
val b = s.incl(x6).incl(x7).incl(x8).incl(x9).incl(x10)

val ab = a union b


val list: TweetList = ab.descendingByRetweet

toString(list)

def toString(t: TweetList): Unit = {
  if(t.isEmpty) println("end")
  else {
    println(t.head)
    toString(t.tail)
  }
}


val ex = new Empty
ex.mostRetweeted