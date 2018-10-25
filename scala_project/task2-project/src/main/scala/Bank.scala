import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)
    



    def createAccount(initialBalance: Double): ActorRef = {
        return BankManager.createAccount("%04d".format(accountCounter.addAndGet(1)), bankId, initialBalance)
        //return context.actorOf(Account.props("%04d".format(accountCounter.getAndAdd(1)), bankId, initialBalance), "Account")
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        try{
            return Some(BankManager.findAccount(bankId, accountId))
        } catch {
            case _: NoSuchElementException => return None
        }
        //return Util.toOption[ActorRef](BankManager.findAccount(bankId, accountId))
        
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        try {
            return Some(BankManager.findBank(bankId))
        } catch {
            case _: NoSuchElementException => return None
        }
        
        //return Util.toOption[ActorRef](BankManager.findBank(bankId))
    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance)
        case GetAccountRequest(id) => {
            sender ! (findAccount(id) match {
                case Some(account) => account
                case None => null
            })
        }
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => {
            val toBankId = t.toAccountNumber.substring(0, 4)
            val toAccountId = t.toAccountNumber.substring(4)

            if (toBankId != bankId) {
                findOtherBank(toBankId).get ! t
            } else {
                findAccount(toAccountId).get ! t
            }

        }

        case msg => throw new RuntimeException("Lmao dude wtf")
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        val isInternal = t.to.length <= 4
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = if (isInternal) t.to else t.to.substring(4)
        val transactionStatus = t.status
        
        if (toBankId != bankId) {
            findOtherBank(toBankId) match {
                case Some(bank) => bank ! t
                case None => {
                    // This account should always exist
                    t.status = TransactionStatus.FAILED
                    findAccount(t.from.substring(4)).get ! new TransactionRequestReceipt(t.from, t.id, t) 
                
                    //
                }
            }
        } else {
            findAccount(toAccountId) match {
                case Some(account) => account ! t
                case None => {
                    // This account should always exist
                    t.status = TransactionStatus.FAILED
                    val transactionReceipt = new TransactionRequestReceipt(t.from, t.id, t)
                    if (isInternal){
                        findAccount(t.from.substring(4)).get ! transactionReceipt
                    } else {
                        findOtherBank(t.from.substring(0, 4)).get ! transactionReceipt
                    }
                }
            }
        }
    }
}