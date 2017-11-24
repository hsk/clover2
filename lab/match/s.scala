package s

object main extends App {
val r = """^([a-z]+|[+\-*/]|[0-9]+)(.*)""".r
def l:String=>List[String] = {
case r(a,b) => a::l(b)
case e => List(e) 
}

def l2(a:String):List[String]={
  r.unapplySeq(a) match {
  case s:Some[List[String]] => s.get.(0)::l2(s.get.(1))
  case None => List(a)
  }
}

println(l("a+b/2+5"))
println(l2("a+b/2+5"))
}
