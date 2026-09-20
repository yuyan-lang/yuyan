(func $m762
(param $a0 (ref $tuple))
(param $i1 i64)
(param $i2 i64)
(result (ref null eq)) (local $u (ref null eq)) (local $j i64)

(local $a1 (ref null eq))
(local $a2 (ref null eq))
(local $a3 (ref null eq))
(local $a4 (ref null eq))
(local $a5 (ref null eq))
(local $a6 (ref null eq))
(local $a7 (ref null eq))
(local $a8 (ref null eq))
(local $a9 (ref null eq))
(local $a10 (ref null eq))
(local $a11 (ref null eq))
(local $a12 (ref null eq))
(local $a13 (ref null eq))
(local $a14 (ref null eq))
(local $a15 (ref null eq))
(local $a16 (ref null eq))
(local $a17 (ref null eq))
(local $a18 (ref null eq))
(local $a19 (ref null eq))
(local $a20 (ref null eq))
(local $a21 (ref null eq))

(local $i4 i64)
(local $i5 i64)
(local $i6 i64)










(local $i17 i64)

(local $i19 i64)


(loop $self (result (ref null eq))
local.get $i2
local.get $i1
i64.gt_s i64.extend_i32_u
i32.wrap_i64 ref.i31
local.set $a3
local.get $a3

ref.cast (ref i31) i31.get_u if (result (ref null eq))
local.get $i2
local.get $i1
i64.sub

local.set $i4
local.get $i4
i64.const 2
i64.div_s
local.set $i5
local.get $i1
local.get $i5
i64.add

local.set $i6
local.get $a0

ref.cast (ref $tuple)
local.get $i6
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $tuple
local.set $a7
local.get $a0

ref.cast (ref $tuple)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $tuple
local.set $a8
local.get $a0

ref.cast (ref $tuple)
local.get $i6
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
local.get $a8

array.set $tuple ref.null eq
local.set $a9
local.get $a0

ref.cast (ref $tuple)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
local.get $a7

array.set $tuple ref.null eq
local.set $a10
local.get $a0

ref.cast (ref $tuple)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $tuple
local.set $a11


local.get $a0
local.get $i2
local.get $a11
call $unbox
local.get $i1
local.get $i1
call $m756
local.set $a12
local.get $a0

ref.cast (ref $tuple)
local.get $a12

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $tuple
local.set $a13
local.get $a0

ref.cast (ref $tuple)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $tuple
local.set $a14
local.get $a0

ref.cast (ref $tuple)
local.get $a12

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
local.get $a14

array.set $tuple ref.null eq
local.set $a15
local.get $a0

ref.cast (ref $tuple)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
local.get $a13

array.set $tuple ref.null eq
local.set $a16
local.get $a12

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.const 1
i64.sub

local.set $i17
local.get $a0
 ref.cast (ref $tuple)
local.get $i1
local.get $i17

call $m762
local.set $a18
local.get $a12

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.const 1
i64.add

local.set $i19
local.get $a0
 ref.cast (ref $tuple)
local.get $i19
local.get $i2

local.set $i2
local.set $i1
local.set $a0
br $self
else
ref.null eq

end return
) )