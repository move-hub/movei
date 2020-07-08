//! width: 80
//! indent: 4

module A {
    public fun new_preburn<CoinType>(): Preburn<CoinType> {
        assert(
            Signer::address_of(config_account) ==
                CoreAddresses::LIBRA_ROOT_ADDRESS() + LIBRA_ROOT_ADDRESS(),
            0,
        );
        assert(
            Signer::address_of(config_account) ==
                CoreAddresses::LIBRA_ROOT_ADDRESS(),
            0,
        );
    }
}

