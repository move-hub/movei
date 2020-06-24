script {
use 0x1::HelloWorld;
fun say_hi(receiver: address) {
    HelloWorld::hi(receiver);
}
}