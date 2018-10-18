import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = {
        val diff = balance.amount - amount
        if(amount < 0){
            throw new IllegalAmountException
        } else if(diff < 0) {
            throw new NoSufficientFundsException
        }
        balance.amount = diff
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

    def transferTo(account: Account, amount: Double) = {
        bank.addTransactionToQueue(this, account, amount)
    }


}
