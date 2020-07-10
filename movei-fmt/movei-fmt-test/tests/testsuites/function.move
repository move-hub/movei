//! width: 60
//! indent: 4

module A {
    public fun new_preburn<CoinType>(): Preburn<CoinType> {
        assert_is_coin<CoinType>();
        Preburn<CoinType> { requests: Vector::empty() }
    }

    public fun grant_privileges(account: &signer) {
        Roles::add_privilege_to_account_treasury_compliance_role(
            account,
            RegisterNewCurrency {},
        );
    }
}

//! new-fmt
//! width: 50
//! indent: 4
module A {
    public fun new_preburn<
        CoinType,
    >(): Preburn<CoinType> {
        assert_is_coin<CoinType>();
        Preburn<CoinType> {
            requests: Vector::empty(),
        }
    }

    public fun grant_privileges(
        account: &signer,
    ) {
        Roles::add_privilege_to_account_treasury_compliance_role(
            account,
            RegisterNewCurrency {},
        );
    }
}

//! new-fmt
//! width: 100
//! indent: 4

module A {
    public fun new_preburn<CoinType>(): Preburn<CoinType> {
        assert_is_coin<CoinType>();
        Preburn<CoinType> { requests: Vector::empty() }
    }

    public fun grant_privileges(account: &signer) {
        Roles::add_privilege_to_account_treasury_compliance_role(account, RegisterNewCurrency {});
    }

    public fun join<CoinType: resource>(
        coin1: Coin<CoinType>,
        coin2: Coin<CoinType>,
    ): Coin<CoinType> {
        deposit(&mut coin1, coin2);
        coin1
    }
}

//! new-fmt
//! indent: 4
module A {
    public fun burn<CoinType>(account: &signer, preburn_address: address)
    acquires BurnCapability, CurrencyInfo, Preburn {
        burn_with_capability(
            preburn_address,
            borrow_global<BurnCapability<CoinType>>(
                Signer::address_of(account1),
            ),
            borrow_global<BurnCapability<CoinType>>(Signer::address_of(account)),
        )
    }
}