;; 文言：此乃实产之片，非全模块。汉语：实际斐波那契整数内工；外部引用包装入口保留。
(func $n749
(param $i0 i64)
(result i64)
(local $i1 i64)
(local $i2 i64)
(local $i3 i64)
(local $i4 i64)
(local $i5 i64)
(local $i6 i64)
(local $i7 i64)
(local $acc i64) (loop $sum_loop (result i64)
i64.const 2
local.get $i0
i64.gt_s i64.extend_i32_u
local.set $i1
local.get $i1
i64.const 0 i64.ne if (result i64)
local.get $i0
else
local.get $i0
i64.const 1
i64.sub
local.set $i2
local.get $i2
call $n749
local.set $i3
local.get $i0
i64.const 2
i64.sub
local.set $i4
local.get $acc
local.get $i3
i64.add local.set $acc
local.get $i4
local.set $i0
br $sum_loop
end local.set $i7
local.get $i7
) local.get $acc i64.add)
