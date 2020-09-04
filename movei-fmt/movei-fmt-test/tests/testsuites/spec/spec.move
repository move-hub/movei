address 0x1 {
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
        modifies global<Window<CoinType>>(addr);
    }

    spec schema AbortsIfParentIsNotParentVASP {
        local addr: address;
        aborts_if [export] !spec_is_parent_vasp(spec_parent_address(addr));
        aborts_if
            !exists<LimitsDefinition<CoinType>>(limit_address)
        with
            Errors::NOT_PUBLISHED;
        aborts_if
            exists<Window<CoinType>>(Signer::spec_address_of(to_limit))
        with
            Errors::ALREADY_PUBLISHED;
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

    spec fun split {
        aborts_if token.value < amount;
        // TODO: ensure result
    }

    spec fun scaling_factor {
        // Todo: fix name_of()
        pragma verify = false;
        //let x  = name_of();
        //aborts_if !exists<TokenInfo<TokenType>>(x);
    }

    spec module {
        pragma verify = true, aborts_if_is_partial = true;
        // asfd
        pragma
            verify = true,
            aborts_if_is_partial = true,
            aborts_if_is_partial = true;
        // sdfvsdf
        //sfd
        pragma verify = true, aborts_if_is_partial = true;
        // asfd
    }
}
}