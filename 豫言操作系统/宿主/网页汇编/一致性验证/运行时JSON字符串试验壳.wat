;; 文言：低层试验壳，附于 运行时.wat 之后成完整模块，直验 $json_string。
;; 汉语：测试用的低层外壳。输入字节放在线性内存 [0,len)，decode 调用运行时的 $json_string，把解码字节写到 mem[out..]，返回解码字节数；已消费的源字节数（含首尾引号）存于导出全局 consumed。
(memory (export "mem") 512)
(global $consumed (export "consumed") (mut i32) (i32.const 0))
(func $h_load (param $len i32) (result (ref $bytes))
 (local $a (ref $bytes)) (local $i i32)
 local.get $len array.new_default $bytes local.set $a
 block $d loop $l
 local.get $i local.get $len i32.ge_u br_if $d
 local.get $a local.get $i local.get $i i32.load8_u array.set $bytes
 local.get $i i32.const 1 i32.add local.set $i br $l end end
 local.get $a)
(func (export "decode") (param $len i32) (param $start i32) (param $out i32) (result i32)
 (local $t (ref $tuple)) (local $r (ref $bytes)) (local $i i32) (local $n i32)
 local.get $len call $h_load
 local.get $start i64.extend_i32_u call $box
 call $json_string
 ref.cast (ref $tuple) local.set $t
 local.get $t i32.const 0 array.get $tuple ref.cast (ref $bytes) local.set $r
 local.get $t i32.const 1 array.get $tuple call $unbox i32.wrap_i64 global.set $consumed
 local.get $r array.len local.set $n
 block $d loop $l
 local.get $i local.get $n i32.ge_u br_if $d
 local.get $out local.get $i i32.add local.get $r local.get $i array.get_u $bytes i32.store8
 local.get $i i32.const 1 i32.add local.set $i br $l end end
 local.get $n)
