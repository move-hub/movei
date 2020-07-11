address 0x1 {
module HelloWorld {
  resource struct T {
    receiver: address,
  }

  public fun hi(signer: &signer, receiver: address) {
      let t = T{ receiver };
      move_to(signer, t);
  }
}
}