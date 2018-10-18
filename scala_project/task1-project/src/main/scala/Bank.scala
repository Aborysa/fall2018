import java.util.UUID
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext




class Bank(val allowedAttempts: Integer = 3) {

    private val uid = UUID.randomUUID
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = ExecutionContext.fromExecutor(new ForkJoinPool())

    private var nextAccountId = new AtomicInteger()


    private var currentProcessingThread : Thread = null

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = synchronized {
        transactionsQueue push new Transaction(
            transactionsQueue, processedTransactions, from, to, amount, allowedAttempts
        )
        executorContext.execute(transactionsQueue.pop)
                        
        /*if (currentProcessingThread == null || !currentProcessingThread.isAlive) {
            processTransactions
        }*/
    }

    // Hint: use a counter
    def generateAccountId: Int = {
        val id = nextAccountId.addAndGet(1)
        return (id - 1)
    }

    private def processTransactions: Unit = {
        currentProcessingThread = new Thread(new Runnable(){
            def run(){
                var waitPasses = 0
                while(waitPasses < 10){
                    while(!transactionsQueue.isEmpty){
                        waitPasses += 1
                    }
                    Thread.sleep(100)
                }
            }
        })
        currentProcessingThread.start
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = synchronized {
        processedTransactions.iterator.toList
    }

}
