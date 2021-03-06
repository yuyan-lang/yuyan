## 关于对象的内存表示方法 

（由于性能原因，考虑不采用本方法）


豫言目前仅支持编译64位程序。

每一个对象将根据类型占用一定的存储空间，具体占用如下。

一个对象的分配至少占用8字节空间（一个指针大小），
例如一个字符串指针（char*)为8字节大小，另外每个对象在分配时都会额外分配8字节大小的头信息（Header Info）。 

头信息的具体内容如下：
1. 前2比特 -- 留空，不需要区分有符号的移位（signed shift）与逻辑移位（logical shift）。
1. 之后6比特 -- 类型信息，其中
    * 1 对应函数闭包 （function closure），主要用于编译带有自由变量（free variables）的函数
    * 2 对应卷（fold），为同构递归类型（iso-recursive types）的对象
    * 3 对应与（tuple），为乘积类型（product types）的对象
    * 4 对应临（injection)，为和类型（sum types）的对象
    * 5 对应元（unit），为单元类型（unit type）的对象
    * 6 对应字符串（string），相当于包裹的C字符串指针（boxed char* pointer）
    * 7 对应包裹的整数（boxed int）
    * 8 对应包裹的浮点数（boxed double）
    * 9 对应引用（reference/pointer)
    * 10 对应数组引用（array reference/pointer)

2. 之后的16比特 -- 这个信息块的长度（不包括所有的头信息块）
3. 之后的40比特 -- 位掩码（bitmask），1 表示对应的信息块是另外一个豫言对象，0 表示信息块不是豫言对象。若块长超过40，则在信息块之后设置额外的位掩码块，每块可以装62个信息块的信息。 （位掩码信息原初设计时用于GC，但后来采用libgc方案做内存回收，这个信息的用处就不大了，可以考虑删去）。

#### 图片说明一个对象的内存占用
```
头信息                           信息块（长度3）额外信息        
[ 2 | 6 - | 16 - - | 40 - - - - | - | - | - | ... | ]
留空  类型   长度      位掩码       信息1 2  3   额外位掩码
```