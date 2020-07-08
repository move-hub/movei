//! width: 80
module A {
    public fun new_preburn<CoinType>(): Preburn<CoinType> {
        if (!info.is_synthetic) {
            Event::emit_event(
                &mut info.mint_events,
                MintEvent { amount: value, currency_code },
            );
            Event::emit_event(
                &mut info.mint_event,
                MintEvent {
                    currency_code,
                    currency_code: a,
                    currency_code: b,
                    currency_code: c,
                },
            );
        };
    }
}
