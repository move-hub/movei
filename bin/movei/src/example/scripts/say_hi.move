script {
use 0x1::HelloWorld;
fun say_hi(signer: &signer, receiver: address) {
    HelloWorld::hi(signer, receiver);
}
}