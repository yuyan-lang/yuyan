
# 对于WASM支持

LLVM代码生成假设所有使用的指针都是64位的，故不可以生成wasm32程序。

libgc支持wasm，但不支持wasm64.

我们需要等待libgc支持wasm64，在这之前，我们的在线IDE不使用libgc，而直接不进行垃圾回收。