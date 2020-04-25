use 0x1::HelloWorld;
fun main(receiver: address) {
    HelloWorld::hi(receiver);
}