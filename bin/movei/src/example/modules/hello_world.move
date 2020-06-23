address 0x1:
module HelloWorld {
  resource struct T {
    receiver: address,
  }

  public fun hi(receiver: address) {
      let t = T{ receiver };
      move_to_sender(t);
  }
}