let f (just: obj) =
    let j = just :?> (obj -> obj)
    j (box 42)
