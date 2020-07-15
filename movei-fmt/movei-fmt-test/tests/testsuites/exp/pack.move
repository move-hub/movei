//! width: 80
//! indent: 4

module A {
    public fun new_preburn<CoinType>(): Preburn<CoinType> {
        Preburn {
            // coins.
            coin: Coin::T { value: 10 },
            // addr.
            addr: 0x1,
        }
    }
}
