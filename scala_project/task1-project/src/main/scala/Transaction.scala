import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
    private var queue: mutable.Queue[Transaction] = new mutable.Queue()
    // Remove and return the first element from the queue
    def pop: Transaction = synchronized {
        return queue.dequeue
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {
        return queue.isEmpty
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = synchronized {
        queue += t
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = {
        return queue.front
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = synchronized {
        return queue.iterator
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {
        def doTransaction() = {

            val acc1Balance = from.getBalanceAmount
            val acc2Balance = to.getBalanceAmount
            
            try {
                from withdraw amount
                to deposit amount
                status = TransactionStatus.SUCCESS
            } catch {
                case e : Exception => {
                    from deposit (acc1Balance - from.getBalanceAmount)
                    from withdraw (to.getBalanceAmount - acc2Balance)
                    status = TransactionStatus.FAILED
                    if (allowedAttemps > 0) {
                        var newTransaction = new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttemps - 1)
                        transactionsQueue.push(newTransaction)
                    }
                }
            }
            processedTransactions.push(this)
        }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
      } else to synchronized {
          from synchronized {
            doTransaction
          }
      }

      // Extend this method to satisfy requirements.

    }
}
