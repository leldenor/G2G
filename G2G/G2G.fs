module G2G

type Size = Small | Medium | Large

type DrinkType = Coffee | Tea | Juice

type Drink = {Type : DrinkType; Size : Size; Price: float}

type Product = {ProductType : Drink; Amount : float}

type VIAPerson = {Name : string}
type SOSUPerson = {Name : string}
type Customer = VIAPerson | SOSUPerson

//type CreditCard = {Number:int}
//type Cash = {Amount: int}
//type MobilePay = {}
type Payment = CreditCard | Cash | MobilePay

type Order = {ProductList: Product[]; Payment: Payment; Customer: Customer}

let Latte = {Type = Coffee; Size = Small; Price = 15.00}
let Pepermint = {Type = Tea; Size = Medium; Price = 10.00}
let Apple = {Type = Juice; Size = Large; Price = 25.00}

let product = {ProductType = Latte; Amount = 2.0}
let ProductList = [Latte; Latte; Apple]
let sumProd = product.ProductType.Price * product.Amount
let sum = Latte.Price + Pepermint.Price

let rec calculateProducts lst =
    let rec calculateHelper (lst, total) = // total = accumulator value
        match lst with
            | [] -> total
            | hd::tl -> 
                let ntotal = hd.Price + total
                calculateHelper(tl, ntotal)
    calculateHelper(lst, 0)
    

