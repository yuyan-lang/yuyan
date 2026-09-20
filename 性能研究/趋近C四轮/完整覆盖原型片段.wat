
;; 文言：仅供有界全写消融，未入编译器。汉语：固定单缓冲、128 MiB 上限；该基准先覆盖全部元素再读取，故省去显式清零。尚未实现通用调用图验证与超限回退。
(memory $scratch 1 2048)
(global $arena_len (mut i32) (i32.const 0))
(func $arena_new (param $v i64) (param $n i32) (result i32) (local $pages i32)
 local.get $n i32.const 16777215 i32.gt_u if unreachable end
 local.get $n global.set $arena_len
 local.get $n i32.const 3 i32.shl i32.const 65543 i32.add i32.const 16 i32.shr_u memory.size i32.sub local.tee $pages i32.const 0 i32.gt_s
 if local.get $pages memory.grow i32.const -1 i32.eq if unreachable end end
 i32.const 0)

(func $f778 (type $t2)
(param $a0 (ref null eq))
(param $a1 (ref null eq))
(result (ref null eq)) (local $u (ref null eq)) (local $j i64)


(local $a2 i32)
(local $a3 (ref null eq))
(local $a4 (ref null eq))






(loop $self (result (ref null eq))
i64.const 0

local.get $a1

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.tee $j i64.const 4294967295 i64.gt_u if unreachable end local.get $j i32.wrap_i64
call $arena_new
local.set $a2
local.get $a2
 
local.get $a1


call $c771
local.set $a3
local.get $a2
 
local.get $a1

i32.const 0 ref.i31


return_call $c774
) )


