
;; 文言：此唯消融之器，未可泛施。汉语：仅针对该排序产物的私有线性内存实验，容量与异常清理尚未推广。
(memory $scratch 1)
(global $cursor (mut i32) (i32.const 0))
(func $pnew (param $v i64) (param $n i32) (result (ref i31)) (local $p i32) (local $e i64) (local $pages i32) (local $i i32)
 global.get $cursor local.tee $p i64.extend_i32_u
 local.get $n i64.extend_i32_u i64.const 8 i64.mul i64.add i64.const 8 i64.add local.tee $e
 i64.const 2147483647 i64.gt_u if unreachable end
 local.get $e i64.const 65535 i64.add i64.const 16 i64.shr_u i32.wrap_i64 memory.size i32.sub local.tee $pages i32.const 0 i32.gt_s
 if local.get $pages memory.grow i32.const -1 i32.eq if unreachable end end
 local.get $e i32.wrap_i64 global.set $cursor
 local.get $p local.get $n i32.store
 local.get $v i64.eqz
 if local.get $p i32.const 8 i32.add i32.const 0 local.get $n i32.const 3 i32.shl memory.fill
 else block $done loop $fill local.get $i local.get $n i32.ge_u br_if $done
 local.get $p local.get $i i32.const 3 i32.shl i32.add local.get $v i64.store offset=8
 local.get $i i32.const 1 i32.add local.set $i br $fill end end end
 local.get $p ref.i31)
(func $pget (param $p (ref i31)) (param $i i32) (result i64)
 local.get $i local.get $p i31.get_u i32.load i32.ge_u if unreachable end
 local.get $p i31.get_u local.get $i i32.const 3 i32.shl i32.add i64.load offset=8)
(func $pset (param $p (ref i31)) (param $i i32) (param $v i64)
 local.get $i local.get $p i31.get_u i32.load i32.ge_u if unreachable end
 local.get $p i31.get_u local.get $i i32.const 3 i32.shl i32.add local.get $v i64.store offset=8)

(func $f766 (type $t2)
(param $a0 (ref null eq))
(param $a1 (ref null eq))
(result (ref null eq)) (local $u (ref null eq)) (local $j i64)


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


(local $i4 i64)

(local $i6 i64)
(local $i7 i64)
(local $i8 i64)
(local $i9 i64)
(local $i10 i64)
(local $i11 i64)
(local $i12 i64)
(local $saved i32) global.get $cursor local.set $saved
(loop $self (result (ref null eq))
i64.const 0
local.get $a1

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
call $pnew
local.set $a2
local.get $a2
 ref.cast (ref i31)
local.get $a1

i32.const 0 ref.i31


call $c752
local.set $a3
local.get $a1

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.const 1
i64.sub

local.set $i4
local.get $a2
 ref.cast (ref i31)
i64.const 0
local.get $i4

call $d762
local.set $a5
local.get $a2

ref.cast (ref i31)
i64.const 0
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
call $pget
local.set $i6
local.get $a1

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.const 2
i64.div_s
local.set $i7
local.get $a2

ref.cast (ref i31)
local.get $i7
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
call $pget
local.set $i8
local.get $i6
local.get $i8
i64.add

local.set $i9
local.get $a1

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.const 1
i64.sub

local.set $i10
local.get $a2

ref.cast (ref i31)
local.get $i10
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
call $pget
local.set $i11
local.get $i9
local.get $i11
i64.add

local.set $i12
local.get $saved global.set $cursor
local.get $i12 call $box
) )