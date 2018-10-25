import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()


object Account {
    def props(accountId: String, bankId: String, initialBalance: Double = 0): Props = Props(new Account(accountId, bankId, initialBalance))
}


class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        return transactions.values.toList
    }

    def allTransactionsCompleted: Boolean = {
        return getTransactions.foldLeft(true)(
            (a, b) => a && b.isCompleted
        )
    }

    def withdraw(amount: Double): Unit = {
        val diff = balance.amount - amount
        if(amount < 0){
            throw new IllegalAmountException
        } else if(diff < 0) {
            throw new NoSufficientFundsException
        }
        balance.amount -= amount
    } 

    def deposit(amount: Double): Unit = {
        if(amount < 0){
            throw new IllegalAmountException
        }
        balance.amount += amount
    } 

    def getBalanceAmount: Double = {
        return balance.amount
    }

    def sendTransactionToBank(t: Transaction): Unit = {
        BankManager.findBank(bankId) ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
        case IdentifyActor => sender ! this

        case TransactionRequestReceipt(to, transactionId, transaction) => {
            // something is wrong if this isn't true
            if (to != getFullAddress || !transactions.contains(transactionId)){
                throw new RuntimeException("Recieved a receipt for a transaction that was never sendt %s != %s || %s -> %b".format(to, getFullAddress, transactionId, transactions.contains(transactionId)))
            }
            // localTransaction may or may not? be the same object as transaction
            val localTransaction = transactions(transactionId)
            if(transaction.status == TransactionStatus.FAILED) {
                deposit(localTransaction.amount)
                localTransaction.status = TransactionStatus.FAILED
            } else {
                localTransaction.status = TransactionStatus.SUCCESS
            }

            localTransaction.receiptReceived = true
            

        }

        case BalanceRequest => sender ! getBalanceAmount

        case t: Transaction => {
            try {
                deposit(t.amount)
            } catch {
                case _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }

            sender ! new TransactionRequestReceipt(t.from, t.id, t)

        }

        case msg => throw new RuntimeException("Lmao dude wtf")
    }


}
