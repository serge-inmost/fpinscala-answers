package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    val x = List(1,2,3,4,5) match {                         							// ex 3.1
        case Cons(x, Cons(2, Cons(4, _))) => x              							//no match
        case Nil => 42                                      							//no match
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y 							//matching with 3
        case Cons(h, t) => h + sum(t)                       							//matching with 15
        case _ => 101                                       							//matching with 101
    }

    def tail[A](as: List[A]): List[A] = as match {          							// ex 3.2
        case Nil => Nil
        case Cons(_,t) => t
    }    

    def setHead[A](as: List[A], a:A): List[A] = as match {  							// ex 3.3
        case Nil => Nil
        case Cons(_,t) => Cons(a,t)
    } 

    def drop[A](l: List[A], n: Int): List[A] = {             							// ex 3.4
        def loop(l: List[A], counter: Int): List[A] =
            if (counter <= 0) l
            else l match {
                case Nil => Nil
                case Cons(_,t) => loop(t,counter-1)
            } 
        loop(l,n)
	}

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {  				// ex 3.5
		case Nil => Nil
		case Cons(h,t) =>   if (f(h)) dropWhile(t,f)
                            else l 
    }    
	
	def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
		case Nil => a2
		case Cons(h,t) => Cons(h, append(t, a2))
	}
		
    def init[A](l: List[A]): List[A] = l match {            							// ex 3.6
		case Nil => sys.error("init of empty list") 									// to prevent the warning (Match Non-exhaustive)
        case Cons(_,Nil) => Nil
        case Cons(h,t) => Cons(h,init(t))
	}
	
	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}
	
	def length[A](as: List[A]): Int =													// ex 3.9
		foldRight(as, 0)((x,y) => y+1)
		
	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {  				// ex 3.10
			case Nil => z
			case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
	}

	def sumL(ns: List[Int]) =															// ex 3.11
		foldLeft(ns, 0)((x,y) => x + y)
		
	def productL(ns: List[Double]) =													// ex 3.11
		foldLeft(ns, 1.0)(_ * _)

	def lengthL[A](as: List[A]): Int =													// ex 3.11
		foldLeft(as, 0)((x,y) => x + 1)
		
	def reverse[A](as: List[A]): List[A] =												// ex 3.12
		foldLeft(as, List[A]())((acc,h) => Cons(h,acc))
		
	def appendL[A](a1: List[A], a2: List[A]): List[A] =									// ex 3.14
		foldRight(a1, a2)(Cons(_,_))
		
	def concat[A](l: List[List[A]]): List[A] = 											// ex 3.15
		foldRight(l, Nil:List[A])(append)
		
	def addone(ns: List[Int]): List[Int] =												// ex 3.16
		foldRight(ns, Nil:List[Int])((x,y) => Cons(x+1,y))
		
	def dToString(ds: List[Double]): List[String] =										// ex 3.17
		foldRight(ds, Nil:List[String])((x,y) => Cons(x.toString,y))
	
	def map[A,B](as: List[A])(f: A => B): List[B] =										// ex 3.18
		foldRight(as, Nil:List[B])((x,y) => Cons(f(x),y))
		
	def filter[A](as: List[A])(f: A => Boolean): List[A] =								// ex 3.19
		foldRight(as, Nil:List[A])((x,y) => if (f(x)) Cons(x,y) else y)
	
	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =							// ex 3.20
		concat(map(as)(f))
	
	def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =					// ex 3.21
		flatMap(as)(a => if (f(a)) List(a) else Nil)
	
	def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {				// ex 3.22
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addLists(t1,t2))
	}
	
	def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1,a2) match {	// ex 3.23
		case (Nil, _) => Nil
		case (_, Nil) => Nil
		case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
	}

	@annotation.tailrec
	def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match { 		// ex 3.24
	  case (_,Nil) => true
	  case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
	  case _ => false
	}
	@annotation.tailrec
	def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match { 			// ex 3.24
	  case Nil => sub == Nil
	  case _ if startsWith(sup, sub) => true
	  case Cons(_,t) => hasSubsequence(t, sub)
	}
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	def size[A](t: Tree[A]): Int = t match { 											// ex 3.25
 		case Leaf(_) => 1
		case Branch(left,right) => 1 + size(left) + size(right)
	}

	def maximum(t: Tree[Int]): Int = t match { 											// ex 3.26
		case Leaf(i) => i
		case Branch(left,right) => maximum(left) max maximum(right)
	}	
	
	def depth[A](t: Tree[A]): Int = t match { 											// ex 3.27
		case Leaf(_) => 0
		case Branch(left,right) => 1 + (depth(left) max depth(right))
	}
	
	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match { 							// ex 3.28
		case Leaf(a) => Leaf(f(a))
		case Branch(left,right) => Branch(map(left)(f),map(right)(f))
	}
	
	def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match { 					// ex 3.29
		case Leaf(a) => f(a)
		case Branch(left,right) => g(fold(left)(f)(g), fold(right)(f)(g))
	}
	
	def sizeViaFold[A](t: Tree[A]): Int = 												// ex 3.29
 		fold(t)(a => 1)(1 + _ + _)

	def maximumViaFold(t: Tree[Int]): Int = 											// ex 3.29
		fold(t)(a => a)(_ max _)
	
	def depthViaFold[A](t: Tree[A]): Int = 												// ex 3.29
		fold(t)(a => 0)((l,r) => 1 + (l max r))
	
	def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =  								// ex 3.29
		fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
	
}

List.x    																				// ex3.1 - prints 3 as first match

List.filter(List(1,2,3,4,5,6))(x => x % 2 == 0) 										// ex 3.19 - prints List(2,4,6) as expected
