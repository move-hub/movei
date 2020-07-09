module A {
    spec module {
        /// Only mint and burn functions can change the total amount of currency.
        apply TotalValueRemainsSame<CoinType> to *<CoinType>
        except
            mint<CoinType>,
            mint_with_capability<CoinType>,
            burn<CoinType>,
            burn_with_capability<CoinType>,
            burn_with_resource_cap<CoinType>;
        apply SumOfCoinValuesInvariant<CoinType> to *<CoinType>;
        /// Apply invariant from `RegisteredCurrencies` to functions
        /// that call functions in `RegisteredCurrencies`.
        apply RegisteredCurrencies::OnlyConfigAddressHasRegisteredCurrencies
        to
            initialize,
            register_currency<CoinType>
        except
            mint<CoinType>,
            mint_with_capability<CoinType>;
    }

    spec schema SumOfCoinValuesInvariant<CoinType> {
        /// The sum of value of coins is consistent with
        /// the total_value CurrencyInfo keeps track of.
        invariant module
            !spec_is_currency<CoinType>() ==> sum_of_coin_values<CoinType> == 0;
        invariant module
            spec_is_currency<CoinType>() ==>
                sum_of_coin_values<CoinType> ==
                    global<CurrencyInfo<CoinType>>(
                        CoreAddresses::SPEC_CURRENCY_INFO_ADDRESS(),
                    ).total_value;
    }

    spec module {
        pragma verify = true, aborts_if_is_partial = true;
        pragma
            verify = true,
            aborts_if_is_partial = true,
            aborts_if_is_partial = true;
    }
}