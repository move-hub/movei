//! width: 80
//! indent: 4

module A {
    public fun new_preburn<CoinType>(): Preburn<CoinType> {
        let a: A;
        let a: A = A {};
        let A { b: c, a } = aa;
        Preburn {
            // coins.
            coin: Coin::T { value: 10 },
            // addr.
            addr: 0x1,
        }
    }

    public fun destroy_mint_capability<TokenType>(
        cap: MintCapability<TokenType>,
    ) {
        let MintCapability<TokenType> { } = cap;
    }

    spec fun destroy_mint_capability {
    }

    spec fun scaling_factor {
        // Todo: fix name_of()
        pragma verify = false;
        //let x  = name_of();
        //aborts_if !exists<TokenInfo<TokenType>>(x);
    }
}
