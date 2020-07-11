//! account: alice

//! new-transaction
script {
  use 0x1::HelloWorld;
  fun test_hello(signer: &signer) {
    HelloWorld::hi(signer, 0x42);
  }
}

// check: EXECUTED