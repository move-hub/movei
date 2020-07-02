script {
  use 0x1::Debug;

  fun main(_signer: &signer) {
    Debug::print_stack_trace();
  }
}
