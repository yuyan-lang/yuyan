(module
(import "p" "time" (func $time (result f64)))
(import "p" "log" (func $log (param i32 f64)))
(type $octs (array (mut i8)))
(type $big (struct (field i64)))
(func $box (param $n i64) (result (ref null eq))
 local.get $n i64.const -1073741824 i64.ge_s
 local.get $n i64.const 1073741823 i64.le_s i32.and
 if (result (ref null eq))
  local.get $n i32.wrap_i64 ref.i31
 else
  local.get $n struct.new $big
 end)
(func $unbox (param $v (ref null eq)) (result i64)
 local.get $v ref.test (ref i31)
 if (result i64)
  local.get $v ref.cast (ref i31) i31.get_s i64.extend_i32_s
 else
  local.get $v ref.cast (ref $big) struct.get $big 0
 end)
(func $db749
(param $a0 (ref $octs))
(param $i1 i64)
(param $i2 i64)
(param $i3 i64)
(result (ref null eq)) (local $u (ref null eq)) (local $j i64)

(local $a1 (ref null eq))
(local $a2 (ref null eq))
(local $a3 (ref null eq))
(local $a4 (ref null eq))
(local $a5 (ref null eq))
(local $a6 (ref null eq))
(local $a7 (ref null eq))
(local $a8 (ref null eq))


(local $i6 i64)


(local $p4 i32)




(loop $self (result (ref null eq))
local.get $i3
local.get $i1
i64.gt_s
local.set $p4
local.get $p4
if (result (ref null eq))
ref.null eq

else
local.get $a0

ref.cast (ref $octs)
local.get $i3
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
i32.const 0 ref.i31

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i32.wrap_i64 array.set $octs ref.null eq
local.set $a5
local.get $i3
local.get $i2
i64.add

local.set $i6
local.get $a0
 ref.cast (ref $octs)
local.get $i1
local.get $i2
local.get $i6

local.set $i3
local.set $i2
local.set $i1
local.set $a0
br $self
end return
)
)
(func $db754
(param $a0 (ref $octs))
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
(local $i3 i64)

(local $i5 i64)
(local $i6 i64)


(local $i9 i64)



(local $p4 i32)







(loop $self (result (ref null eq))
local.get $i2
local.get $i2
i64.mul

local.set $i3
local.get $i3
local.get $i1
i64.gt_s
local.set $p4
local.get $p4
if (result (ref null eq))
ref.null eq

else
local.get $a0

ref.cast (ref $octs)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get_u $octs i64.extend_i32_u
local.set $i5
local.get $i5 i32.wrap_i64
if (result (ref null eq))
local.get $i2
local.get $i2
i64.mul

local.set $i6
local.get $a0
 ref.cast (ref $octs)
local.get $i1
local.get $i2
local.get $i6

call $db749
local.set $a7
local.get $a7

else
ref.null eq

end local.set $a8
local.get $i2
i64.const 1
i64.add

local.set $i9
local.get $a0
 ref.cast (ref $octs)
local.get $i1
local.get $i9

local.set $i2
local.set $i1
local.set $a0
br $self
end return
)
)
(func $db758
(param $a0 (ref $octs))
(param $i1 i64)
(param $i2 i64)
(param $i3 i64)
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

(local $i5 i64)
(local $i6 i64)
(local $i7 i64)
(local $i8 i64)


(local $p4 i32)






(loop $self (result (ref null eq))
local.get $i2
local.get $i1
i64.gt_s
local.set $p4
local.get $p4
if (result (ref null eq))
local.get $i3 call $box
else
local.get $i2
i64.const 1
i64.add

local.set $i5
local.get $a0

ref.cast (ref $octs)
local.get $i2
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
array.get_u $octs i64.extend_i32_u
local.set $i6
local.get $i6 i32.wrap_i64
if (result i64)
local.get $i3
i64.const 1
i64.add

local.set $i7
local.get $i7
else
local.get $i3
end local.set $i8
local.get $a0
 ref.cast (ref $octs)
local.get $i1
local.get $i5
local.get $i8

local.set $i3
local.set $i2
local.set $i1
local.set $a0
br $self
end return
)
)
(func (export "run") (param $n i64) (result i64) (local $a (ref $octs)) (local $t f64) (local $r (ref null eq))
call $time local.set $t
i32.const 1 local.get $n i32.wrap_i64 i32.const 1 i32.add array.new $octs local.set $a
local.get $a i32.const 0 i32.const 0 array.set $octs
local.get $a i32.const 1 i32.const 0 array.set $octs
i32.const 0 call $time local.get $t f64.sub call $log
call $time local.set $t
local.get $a local.get $n i64.const 2 call $db754 drop
i32.const 1 call $time local.get $t f64.sub call $log
call $time local.set $t
local.get $a local.get $n i64.const 2 i64.const 0 call $db758 local.set $r
i32.const 2 call $time local.get $t f64.sub call $log
local.get $r call $unbox)
)