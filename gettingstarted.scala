package fpinscala.gettingstarted


def fib(n: Int): Int = {
	@annotation.tailrec
	def go(n: Int, second:Int,first:Int): Int = 
		if (n == 1) first
		else
			if (n == 2) second
			else go(n-1,second+first,second)
	go(n,1,0)
}

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
	@annotation.tailrec
	def loop(n: Int): Boolean =
		if (n+1 >= as.length) true
		else if (ordered(as(n),as(n+1))) loop(n + 1)
		else false
	loop(0)
}

//example with integers and their standard order

isSorted(Array(1,2,3),(a:Int,b:Int) => (a <= b)) //prints Boolean=true
isSorted(Array(1,0,3),(a:Int,b:Int) => (a <= b)) //prints Boolean=false

// currying
def curry[A,B,C](f: (A, B) => C): A => (B => C) =
	(a: A) => ((b:B) => f(a,b))

// uncurrying
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
	(a: A, b:B) => f(a)(b)

// function composition
def compose[A,B,C](f: B => C, g: A => B): A => C =
   (a:A) => f(g(a))