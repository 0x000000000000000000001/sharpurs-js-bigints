module JS.BigInt

open System.Numerics
open System

let biSub (x: obj) (y: obj) : obj = box ((x :?> BigInteger) - (y :?> BigInteger))
let biEquals (x: obj) (y: obj) : obj = box ((x :?> BigInteger) = (y :?> BigInteger))
let biCompare (x: obj) (y: obj) : obj = box ((x :?> BigInteger).CompareTo(y :?> BigInteger))
let biDegree (x: obj) : obj = 
    let bx = x :?> BigInteger
    let abs = if bx.Sign < 0 then -bx else bx
    let maxInt = BigInteger(2147483647)
    let min = if abs < maxInt then abs else maxInt
    box (int min)

let biMod (x: obj) (y: obj) : obj =
    let bx = x :?> BigInteger
    let by = y :?> BigInteger
    if by.IsZero then box BigInteger.Zero
    else
        let yy = if by.Sign < 0 then -by else by
        let res = (bx % yy + yy) % yy
        box res

let biDiv (x: obj) (y: obj) : obj =
    let bx = x :?> BigInteger
    let by = y :?> BigInteger
    if by.IsZero then box BigInteger.Zero
    else
        let yy = if by.Sign < 0 then -by else by
        let m = (bx % yy + yy) % yy
        box ((bx - m) / by)

let toNumber (x: obj) : obj = box (float (x :?> BigInteger))
let ``and`` (x: obj) (y: obj) : obj = box ((x :?> BigInteger) &&& (y :?> BigInteger))

let fromTypeLevelInt (x: obj) : obj = box (BigInteger.Parse(x :?> string))

let fromStringImpl (just: obj) (nothing: obj) (s: obj) : obj =
    let str = s :?> string
    if String.IsNullOrEmpty(str) then nothing
    else
        let isNeg = str.[0] = '-'
        let s2 = if isNeg then str.Substring(1) else str
        let mutable radix = 10
        let mutable s3 = s2
        if s2.StartsWith("0x") || s2.StartsWith("0X") then
            radix <- 16
            s3 <- s2.Substring(2)
        elif s2.StartsWith("0b") || s2.StartsWith("0B") then
            radix <- 2
            s3 <- s2.Substring(2)
        elif s2.StartsWith("0o") || s2.StartsWith("0O") then
            radix <- 8
            s3 <- s2.Substring(2)
        
        let mutable valid = true
        let mutable res = BigInteger.Zero
        let radixBig = BigInteger(radix)
        for i = 0 to s3.Length - 1 do
            let c = s3.[i]
            let v = 
                if c >= '0' && c <= '9' then int c - int '0'
                elif c >= 'a' && c <= 'z' then int c - int 'a' + 10
                elif c >= 'A' && c <= 'Z' then int c - int 'A' + 10
                else -1
            if v < 0 || v >= radix then valid <- false
            res <- res * radixBig + BigInteger(v)
            
        if valid && s3.Length > 0 then
            let finalRes = if isNeg then -res else res
            let j = just :?> (obj -> obj)
            j (box finalRes)
        else
            nothing

let fromNumberImpl (just: obj) (nothing: obj) (n: obj) : obj =
    let num = n :?> float
    try
        let res = new BigInteger(num)
        let j = just :?> (obj -> obj)
        j (box res)
    with _ ->
        nothing

let toString (x: obj) : obj = box ((x :?> BigInteger).ToString())
let fromInt (x: obj) : obj = box (BigInteger(x :?> int))

let asIntN (bits: obj) (n: obj) : obj =
    let b = bits :?> int
    let bx = n :?> BigInteger
    let mask = (BigInteger.One <<< b) - BigInteger.One
    let masked = bx &&& mask
    let signBit = BigInteger.One <<< (b - 1)
    if (masked &&& signBit) = signBit then
        box (masked - (BigInteger.One <<< b))
    else
        box masked

let asUintN (bits: obj) (n: obj) : obj =
    let b = bits :?> int
    let bx = n :?> BigInteger
    let mask = (BigInteger.One <<< b) - BigInteger.One
    box (bx &&& mask)

let biAdd (x: obj) (y: obj) : obj = box ((x :?> BigInteger) + (y :?> BigInteger))
let biMul (x: obj) (y: obj) : obj = box ((x :?> BigInteger) * (y :?> BigInteger))
let biOne : obj = box BigInteger.One
let biZero : obj = box BigInteger.Zero
let ``not`` (x: obj) : obj = box (BigInteger.op_OnesComplement(x :?> BigInteger))
let ``or`` (x: obj) (y: obj) : obj = box ((x :?> BigInteger) ||| (y :?> BigInteger))
let pow (x: obj) (y: obj) : obj =
    let bx = x :?> BigInteger
    let by = y :?> BigInteger
    if by.Sign < 0 then box BigInteger.Zero
    else box (BigInteger.Pow(bx, int by))

let shl (x: obj) (y: obj) : obj = box ((x :?> BigInteger) <<< int (y :?> BigInteger))
let shr (x: obj) (y: obj) : obj = box ((x :?> BigInteger) >>> int (y :?> BigInteger))

let toStringAs (radix: obj) (x: obj) : obj =
    let r = radix :?> int
    let mutX = x :?> BigInteger
    if mutX.IsZero then box "0"
    else
        let isNeg = mutX.Sign < 0
        let mutable temp = if isNeg then -mutX else mutX
        let radixBig = BigInteger(r)
        let chars = "0123456789abcdefghijklmnopqrstuvwxyz"
        let mutable res = ""
        while temp.IsZero = false do
            let rem = int (temp % radixBig)
            res <- chars.[rem].ToString() + res
            temp <- temp / radixBig
        if isNeg then box ("-" + res)
        else box res

let xor (x: obj) (y: obj) : obj = box ((x :?> BigInteger) ^^^ (y :?> BigInteger))

let fromStringAsImpl (just: obj) (nothing: obj) (radix: obj) (s: obj) : obj =
    let r = radix :?> int
    let str = s :?> string
    if String.IsNullOrEmpty(str) then nothing
    else
        let isNeg = str.[0] = '-'
        let s2 = if isNeg then str.Substring(1) else str
        let mutable valid = true
        let mutable res = BigInteger.Zero
        let radixBig = BigInteger(r)
        for i = 0 to s2.Length - 1 do
            let c = s2.[i]
            let v = 
                if c >= '0' && c <= '9' then int c - int '0'
                elif c >= 'a' && c <= 'z' then int c - int 'a' + 10
                elif c >= 'A' && c <= 'Z' then int c - int 'A' + 10
                else -1
            if v < 0 || v >= r then valid <- false
            res <- res * radixBig + BigInteger(v)
        
        if valid then
            let finalRes = if isNeg then -res else res
            let j = just :?> (obj -> obj)
            j (box finalRes)
        else
            nothing
