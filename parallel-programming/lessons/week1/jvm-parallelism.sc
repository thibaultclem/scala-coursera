//Copy/paste these snippets on Scala console to test

//Not atomic operation
//Will result:
//Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
//Vector(1, 8, 10, 12, 13, 14, 15, 16, 17, 18)
private var uidCount = 0L

def getUniqueId(): Long = {
  uidCount = uidCount + 1
  uidCount
}

def startThreads() = {
  val t = new Thread {
    override def run() {
      val uids = for (i <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }
  t.start()
  t
}

startThreads();
startThreads();


//Atomic operation
//Will result:
//Vector(1, 4, 6, 7, 8, 10, 12, 14, 16, 18)
//Vector(2, 3, 5, 9, 11, 13, 15, 17, 19, 20)
private var uidCountAtomic = 0L
private val x = new AnyRef {}

def getUniqueIdAtomic(): Long = x.synchronized {
  uidCountAtomic = uidCountAtomic + 1
  uidCountAtomic
}

def startThreadsAtomic() = {
  val t = new Thread {
    override def run() {
      val uids = for (i <- 0 until 10) yield getUniqueIdAtomic()
      println(uids)
    }
  }
  t.start()
  t
}

startThreadsAtomic();
startThreadsAtomic();


/*

Use more fine grained synchronization:
We will start a synchronized block both on the source account and the target
account. This will make sure that the code in the nested block is executed
atomically, both for the threads using the source account, this, and threads
using the target account.

 */

//This program will never complete: Deadlock
class Account(private var amount: Int = 0) {
  def transfer(target: Account, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
}

def startThreadsAccount(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run() {
      for (i <- 0 until n) yield a.transfer(b, 1)
    }
  }
  t.start()
  t
}

val a1 = new Account(500000)
val a2 = new Account(700000)

val t = startThreadsAccount(a1, a2, 150000)
val s = startThreadsAccount(a2, a1, 150000)
t.join()
s.join()


//To solve this, ne approach is to always acquire resources in the same order.
//This assumes an ordering relationship on the resources.
class Account2(private var amount: Int = 0) {

  val uid = getUniqueId()

  private def lockAndTransfer(target: Account2, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }

  def transfer(target: Account2, n: Int) =
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
}