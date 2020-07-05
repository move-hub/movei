//! new-fmt
//! width: 31


script {
  use 0x1::Debug;

  /// A is a const
  const A: u64 = 6;

  /** This is a block comment
  with line break
  */
  fun main<A, B>(s: address) {
    Debug::print_stack_trace();
  }
}
