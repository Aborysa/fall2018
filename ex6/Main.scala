object Hello extends App {
  def sumElemsFor(list: Array[Int]) : Int = {
    var s: Int = 0
    for(i <- list) s += i
    return s
  }

  def sumElemsRec(list: Array[Int]) : Int = {
    if (list.length <= 0) {
      return 0
    }
    val head = list(0)
    return head + sumElemsRec(list.drop(1))
  }

  //BigInt do not have a finite size and can therefore store bigger numbers than regular Int
  def fib(a: BigInt) : BigInt = {
    if(a == 0){return 0}
    if(a == 1){return 1}
    return fib(a-1) + fib(a-2)
  }


  var x: Array[Int] = Array()
  for (i <- Range(1, 51)) x = x :+ i
  x = x ++ Range(51,101)
  println(s"There are ${x.length} elements")
  println(s"Sum of elements are ${sumElemsFor(x)}")
  println(s"Sum of elements are ${sumElemsRec(x)}")
  println(s"fib(10) is ${fib(10)}")


  // Ex 2
  def my_func(f: () => BigInt, b: Boolean) = {
    lazy val t = f()
    if (b) println (t)
  }

  def watcher() : BigInt = {
    println("Called!")
    return 5
  }

  my_func(watcher, false)
  my_func(watcher, true)
  // a) the arguments are f and b. f is any lambda expression that takes no arguments and evaluates to a BigInt
  // b is a boolean
  def my_func_not_lazy(f: () => BigInt, b: Boolean) = {
    val t = f()
    if (b) println (t)
  }

  my_func_not_lazy(watcher, false)
  my_func_not_lazy(watcher, true)

  // b) t is a 'lazy' value, which means it is only evaluated if it is used. In this case it depends on weather or not b is true or false.
  // if b is false t will not be evaluated to what f returns when it is called
  // c) lasy evaluation is useful to not prevent running unessesary and process heavy code when the value is discarded


  // Ex 3
  // a
  def createThread(f: () => {}) : Thread = {
    return new Thread(new Runnable {
      def run(){
        return f()
      }
    })
  }
  // b
  def fibLambda(a: BigInt) : Array[() => BigInt] = {
    var ret : Array[() => BigInt] = Array(() => fib(a))
    if (a <= 0) {return ret}
    return fibLambda(a - 1) ++ ret
  }

  // c
  var fibLambdaThreads = fibLambda(10).map(createThread)
  // d - mapping each thread to thread.start causes the thread to execute
  var mapFibLambdaThreadsToStart = fibLambdaThreads.map((a) => a.start)
  for(l <- mapFibLambdaThreadsToStart) println(l)
  // e
  @volatile private var counter: Int = 0
  def increaseCounter(): Int = {
    counter += 1
    counter
  }


  private var counter2: Int = 0
  val rnd = new scala.util.Random()
  def tfunc1(a: Int): Int = {
    counter += 1
    if (a <= 0) {return 0}
    val r = rnd.nextInt(2)
    if (r <= 2){
      return tfunc2(a-1)
    } else {
      return tfunc1(a-1)
    }
  }

  def tfunc2(a: Int): Int = {
    counter -= 1
    if (a <= 0) {return 0}
    val r = rnd.nextInt(2)
    if (r <= 2){
      return tfunc1(a-1)
    } else {
      return tfunc2(a-1)
    }
  }


  val t1 = new Thread(new Runnable{
    def run(){
      println(tfunc1(10000))
      println(counter)
    }
  })


  val t2 = new Thread(new Runnable{
    def run(){
      println(tfunc2(10000))
      println(counter)
    }
  })

  val t3 = new Thread(new Runnable{
    def run(){
      for(i <- Range(0, 10000000)){
        increaseCounter()
      }
      print(counter)
    }
  })
  t3.start
  t1.start
  t2.start
  // the snippet is not thread safe becuase it modifies shared memory without locking this means two threads can end up modifying the same value at det same time causeing a concurrent modification exception
  //
}
