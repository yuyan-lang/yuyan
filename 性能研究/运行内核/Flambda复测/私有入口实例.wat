;; 文言：此为实产之片，非全模块。汉语：从最终斐波那契基准提取的实际函数片段；函数编号不属于稳定 ABI。
(func $f747 (type $t1)
(param $i0 (ref null eq))
(result (ref null eq))
local.get $i0 call $unbox
call $n747 call $box)

(func $n747
(param $i0 i64)
(result i64)
(local $i1 i64)
(local $i2 i64)
(local $i3 i64)
(local $i4 i64)
(local $i5 i64)
(local $i6 i64)
(local $i7 i64)
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
call $n747
local.set $i3
local.get $i0
i64.const 2
i64.sub
local.set $i4
local.get $i4
call $n747
local.set $i5
local.get $i3
local.get $i5
i64.add
local.set $i6
local.get $i6
end local.set $i7
local.get $i7
)
