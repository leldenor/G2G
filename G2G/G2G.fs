module G2G

type Size = Small | Medium | Large

type DrinkType = Coffee | Tea | Juice

type Drink = {Type : DrinkType; Size : Size; Price: float}

type Product = {ProductType : Drink; Amount : float}

type Customer = VIAPerson | SOSUPerson

type Payment = CreditCard | Cash | MobilePay

type Order = {ProductList: Product list; Payment: Payment; Customer: Customer}

let green = {Type = Tea; Size = Medium; Price = 15.00}
let black = {Type = Tea; Size = Small; Price = 10.00}
let fruit = {Type = Tea; Size = Large; Price = 20.00}

let latte = {Type = Coffee; Size = Medium; Price = 15.00}
let cappuccino = {Type = Coffee; Size = Small; Price = 10.00}
let mocha = {Type = Coffee; Size = Large; Price = 20.00}

let apple = {Type = Juice; Size = Medium; Price = 15.00}
let orange = {Type = Juice; Size = Small; Price = 10.00}
let grape = {Type = Juice; Size = Large; Price = 20.00}

/// ------------------------------------------------------------------------- ///


let gtgVat n x = x * (1.0 + n/100.0)

let rec calculateProducts lst =
    let rec calculateHelper (lst, total) = 
        match lst with
            | [] -> total
            | hd::tl ->
                match hd.ProductType.Type with
                    | Coffee -> 
                        let x = (hd.ProductType.Price + (gtgVat hd.ProductType.Price 2.5)) * hd.Amount + total
                        calculateHelper(tl, x)
                    | _ ->
                       let ntotal = hd.ProductType.Price * hd.Amount + total
                       calculateHelper(tl, ntotal)
    calculateHelper(lst, 0)


let OrderMsgSystem =
    MailboxProcessor.Start(fun inbox ->
        let rec processMessage = 
            async {
                let! msg = inbox.Receive()
                printfn "recieved a order!"
                let newMsg = calculateProducts msg.ProductList
                printfn "processed the order! Please pay %f DKK" newMsg
                return! processMessage 
            }
        processMessage)

let product1 = {ProductType = green; Amount = 2}
let product2 = {ProductType = apple; Amount = 10}
OrderMsgSystem.Post({ProductList = [product2]; Payment = Cash; Customer = VIAPerson})




