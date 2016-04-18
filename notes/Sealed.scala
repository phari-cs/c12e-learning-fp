package com.c12e.learn

// sealed - all known implementation - all extentions of it in here
// two ways to implement
// interfaces can be created in two ways
// 		- class that extends the interface
// 		-


sealed trait MyIface {
  val name: String
}

// method 1 of implementing interface
class MyImple extends MyIface { // would have to implement obligatory methods here if any
  val name = "I have to get implemented because need this to be concrete"
}

// method 2 of implementing interface
// since the trait is sealed, cant use new with the trait outside this class
object MyIface {
  def factory(s: String): MyIface = {
    new MyIface { val name = s }
  }

}
