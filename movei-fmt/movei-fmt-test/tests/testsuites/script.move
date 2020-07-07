//! new-fmt
//! width: 31
//! indent: 2
script {
  use 0x1::Debug;

  fun main<A, B>(s: address) {
    Debug::print_stack_trace();
  }
}

//! new-fmt
//! width: 25
//! indent: 2
script {
  use 0x1::Debug;

  fun main<A, B>(
    s: address,
  ) {
    Debug::print_stack_trace();
  }
}
