def product(f: Int=>Int)(a: Int, b: Int): Int = 
	if a>b then 1 else f(a) * product(f)(a+1, b)
def fac(a: Int): Int = 
	product(x=>x)(1, a)

def calc(base: Int, f: Int=>Int, op: (Int, Int)=>Int)(a: Int, b: Int): Int = // this is essentially a mapReduce
	if a>b then base else op(f(a), calc(base, f, op)(a+1, b))

def calcSum(f: Int=>Int) = calc(0, f, (a,b)=>a+b)
def calcProd(f: Int=>Int) = calc(1, f, (a,b)=>a*b)

fac(5)
calcProd(x=>x)(1,5)
product(x=>x)(1,5)
calcSum(fac)(1,5)