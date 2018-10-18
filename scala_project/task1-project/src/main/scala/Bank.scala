import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = 0
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = ???

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue push new Transaction(
        transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    }

    // Hint: use a counter 
    def generateAccountId: Int = {
        uid += 1
        return uid
    }

    private def processTransactions: Unit = {
        // TODO: Implement
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
