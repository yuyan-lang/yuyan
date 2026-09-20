(func $d756
(param $a0 (ref $ints))
(param $i1 i64)
(param $i2 i64)
(param $i3 i64)
(param $i4 i64)
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

(local $i6 i64)

(local $i8 i64)
(local $i9 i64)


(local $i12 i64)
(local $i13 i64)

(local $i15 i64)



(local $p5 i32)

(local $p7 i32)












local.get $i3
local.get $i1
i64.eq
local.set $p5

local.get $p5
if (result (ref null eq))

local.get $i4 call $box
else
(loop $self (result (ref null eq))
(block $step
local.get $a0

ref.cast (ref $ints)
local.get $i3
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $ints
local.set $i6
local.get $i2
local.get $i6
i64.gt_s
local.set $p7
local.get $p7
if (result (ref null eq))
local.get $a0

ref.cast (ref $ints)
local.get $i3
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $ints
local.set $i8
local.get $a0

ref.cast (ref $ints)
local.get $i4
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get $ints
local.set $i9
local.get $a0

ref.cast (ref $ints)
local.get $i3
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
local.get $i9
array.set $ints ref.null eq
local.set $a10
local.get $a0

ref.cast (ref $ints)
local.get $i4
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
local.get $i8
array.set $ints ref.null eq
local.set $a11
local.get $i3
i64.const 1
i64.add

local.set $i12
local.get $i4
i64.const 1
i64.add

local.set $i13
local.get $a0
 ref.cast (ref $ints)
local.get $i1
local.get $i2
local.get $i12
local.get $i13

local.set $i4
local.set $i3
local.set $i2
local.set $i1
local.set $a0
br $step
else
local.get $i3
i64.const 1
i64.add

local.set $i15
local.get $a0
 ref.cast (ref $ints)
local.get $i1
local.get $i2
local.get $i15
local.get $i4

local.set $i4
local.set $i3
local.set $i2
local.set $i1
local.set $a0
br $step
end return

)

local.get $i3
local.get $i1
i64.eq
local.set $p5

local.get $p5 i32.eqz br_if $self

local.get $i4 call $box
)
end
)