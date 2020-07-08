//! new-fmt
//! width: 31
//! indent: 2

script {
  use 0x1::Debug;
  use 0x1::Vector;

  /// A is a const
  const A: u64 = 6;
  const B: address = 0x1;

  /** This is a block comment
  with line break
  */
  fun main<A, B>(s: address) {
    Debug::print_stack_trace();
  }
}
