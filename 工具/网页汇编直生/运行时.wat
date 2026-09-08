(import "yuyan:gc-host/v1" "call" (func $host (param (ref null eq)) (param (ref null eq)) (result (ref null eq))))
;; 文言：值以引用存，巨整别装，诸物托引擎回收。
;; 汉语：小整数用 i31，完整 i64 用 GC struct；元组、引用、闭包环境用 GC array。
(type $big (struct (field i64)))
(type $tuple (array (mut (ref null eq))))
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
;; 文言：先验索引而后缩位。汉语：拒绝负数及超过 wasm32 的索引，防止截断后访问另一元素。
(func $index (param $n (ref null eq)) (result i32) (local $v i64)
 local.get $n call $unbox local.tee $v i64.const 4294967295 i64.gt_u
 if unreachable end local.get $v i32.wrap_i64)
(func $new (param $v (ref null eq)) (param $n (ref null eq)) (result (ref null eq))
 local.get $v local.get $n call $index array.new $tuple)
(func $get (param $a (ref null eq)) (param $n (ref null eq)) (result (ref null eq))
 local.get $a ref.cast (ref $tuple) local.get $n call $index array.get $tuple)
(func $set (param $v (ref null eq)) (param $n (ref null eq)) (param $a (ref null eq)) (result (ref null eq))
 local.get $a ref.cast (ref $tuple) local.get $n call $index local.get $v array.set $tuple ref.null eq)
;; 文言：以异常归返，常途无续体。汉语：提前退出使用 EH；标记单调递增，禁止绕回后误认失效续延。
(tag $escape (param (ref null eq) (ref null eq)))
(global $escape_counter (mut i32) (i32.const 0))
(func $escape_id (result (ref null eq))
 global.get $escape_counter i32.const 1073741823 i32.ge_u if unreachable end
 global.get $escape_counter i32.const 1 i32.add global.set $escape_counter
 global.get $escape_counter ref.i31)
(type $float (struct (field f64)))
(type $bytes (array (mut i8)))
(global $exception (mut (ref null eq)) (ref.null eq))
(func $new_uninit (param $n (ref null eq)) (result (ref null eq))
 ref.null eq local.get $n call $index array.new $tuple)
(func $set_ref (param $v (ref null eq)) (param $a (ref null eq)) (result (ref null eq))
 local.get $a ref.cast (ref $tuple) i32.const 0 local.get $v array.set $tuple ref.null eq)
(func $byte_get (param $a (ref null eq)) (param $n (ref null eq)) (result (ref null eq))
 local.get $a ref.cast (ref $bytes) local.get $n call $index array.get_u $bytes i64.extend_i32_u call $box)
(func $concat (param $x (ref null eq)) (param $y (ref null eq)) (result (ref null eq))
 (local $a (ref $bytes)) (local $b (ref $bytes)) (local $out (ref $bytes)) (local $n i32) (local $m i32)
 local.get $x ref.cast (ref $bytes) local.tee $a array.len local.set $n
 local.get $y ref.cast (ref $bytes) local.tee $b array.len local.set $m
 local.get $n i32.eqz if local.get $b return end
 local.get $m i32.eqz if local.get $a return end
 local.get $n local.get $m i32.add local.get $n i32.lt_u if unreachable end
 local.get $n local.get $m i32.add array.new_default $bytes local.set $out
 local.get $out i32.const 0 local.get $a i32.const 0 local.get $n array.copy $bytes $bytes
 local.get $out local.get $n local.get $b i32.const 0 local.get $m array.copy $bytes $bytes
 local.get $out)
(func $str_cmp (param $x (ref null eq)) (param $y (ref null eq)) (result i32)
 (local $a (ref $bytes)) (local $b (ref $bytes)) (local $n i32) (local $m i32) (local $i i32) (local $u i32) (local $v i32)
 local.get $x ref.cast (ref $bytes) local.tee $a array.len local.set $n
 local.get $y ref.cast (ref $bytes) local.tee $b array.len local.set $m
 block $end loop $loop
 local.get $i local.get $n i32.ge_u br_if $end
 local.get $i local.get $m i32.ge_u br_if $end
 local.get $a local.get $i array.get_u $bytes local.set $u
 local.get $b local.get $i array.get_u $bytes local.set $v
 local.get $u local.get $v i32.ne if
 local.get $u local.get $v i32.sub return end
 local.get $i i32.const 1 i32.add local.set $i br $loop end end
 local.get $n local.get $m i32.gt_u local.get $n local.get $m i32.lt_u i32.sub)
(func $str_eq (param $a (ref null eq)) (param $b (ref null eq)) (result (ref null eq))
 local.get $a local.get $b ref.eq if i32.const 1 ref.i31 return end
 local.get $a ref.cast (ref $bytes) array.len local.get $b ref.cast (ref $bytes) array.len i32.ne
 if i32.const 0 ref.i31 return end
 local.get $a local.get $b call $str_cmp i32.eqz ref.i31)
(func $str_lt (param $a (ref null eq)) (param $b (ref null eq)) (result (ref null eq))
 local.get $a local.get $b call $str_cmp i32.const 0 i32.lt_s ref.i31)
;; 文言：示型于宿主，以同型造值。汉语：宿主由这些原型取得引擎规范化后的 GC 类型，避免构造不兼容的返回对象。
(global (export "yy_bytes") (ref $bytes) (array.new_fixed $bytes 0))
(global (export "yy_tuple") (ref $tuple) (array.new_fixed $tuple 0))
(global (export "yy_big") (ref $big) (struct.new $big (i64.const 0)))
(global (export "yy_float") (ref $float) (struct.new $float (f64.const 0)))
;; 文言：文本之热术留客，不往返巨文。汉语：编译器频繁读取同一源码/JSON 缓冲区，字符串扫描必须在 GC 数组内执行。
(func $slice (param $s (ref $bytes)) (param $from i32) (param $len i32) (result (ref $bytes)) (local $r (ref $bytes))
 local.get $from local.get $s array.len i32.gt_u if unreachable end
 local.get $len local.get $s array.len local.get $from i32.sub i32.gt_u if unreachable end
 local.get $len array.new_default $bytes local.set $r
 local.get $r i32.const 0 local.get $s local.get $from local.get $len array.copy $bytes $bytes local.get $r)
(func $char_byte (param $s (ref null eq)) (param $n (ref null eq)) (result (ref null eq)) (local $i i32) (local $a (ref $bytes))
 local.get $s ref.cast (ref $bytes) local.set $a local.get $n call $index local.set $i
 local.get $i local.get $a array.len i32.eq if (result (ref null eq)) i32.const 0 ref.i31
 else local.get $a local.get $i array.get_u $bytes ref.i31 end)
(func $substring (param $s (ref null eq)) (param $n (ref null eq)) (result (ref null eq)) (local $i i32) (local $a (ref $bytes))
 local.get $s ref.cast (ref $bytes) local.set $a local.get $n call $index local.set $i
 local.get $a local.get $i local.get $a array.len local.get $i i32.sub call $slice)
(func $utf8_end (param $s (ref $bytes)) (param $i i32) (result i32)
 local.get $i local.get $s array.len i32.ge_u if unreachable end
 local.get $i i32.const 1 i32.add local.set $i
 block $end loop $loop
 local.get $i local.get $s array.len i32.ge_u br_if $end
 local.get $s local.get $i array.get_u $bytes i32.const 192 i32.and i32.const 128 i32.ne br_if $end
 local.get $i i32.const 1 i32.add local.set $i br $loop end end local.get $i)
(func $utf8_char (param $s (ref null eq)) (param $n (ref null eq)) (result (ref null eq)) (local $i i32) (local $a (ref $bytes))
 local.get $s ref.cast (ref $bytes) local.set $a local.get $n call $index local.set $i
 local.get $a local.get $i local.get $a local.get $i call $utf8_end local.get $i i32.sub call $slice)
(func $chars (param $s (ref null eq)) (result (ref null eq))
 (local $a (ref $bytes)) (local $i i32) (local $n i32) (local $j i32) (local $end i32) (local $r (ref $tuple))
 local.get $s ref.cast (ref $bytes) local.set $a
 block $done loop $count
 local.get $i local.get $a array.len i32.ge_u br_if $done
 local.get $a local.get $i call $utf8_end local.set $i
 local.get $n i32.const 1 i32.add local.set $n br $count end end
 local.get $n array.new_default $tuple local.set $r i32.const 0 local.set $i
 block $done loop $fill
 local.get $j local.get $n i32.ge_u br_if $done
 local.get $a local.get $i call $utf8_end local.set $end
 local.get $r local.get $j local.get $a local.get $i local.get $end local.get $i i32.sub call $slice array.set $tuple
 local.get $end local.set $i local.get $j i32.const 1 i32.add local.set $j br $fill end end
 local.get $r local.get $n i64.extend_i32_u call $box array.new_fixed $tuple 2)
(func $join (param $list (ref null eq)) (result (ref null eq))
 (local $a (ref $tuple)) (local $s (ref $bytes)) (local $r (ref $bytes)) (local $n i32) (local $i i32) (local $size i64) (local $pos i32) (local $len i32)
 local.get $list ref.cast (ref $tuple) i32.const 0 array.get $tuple ref.cast (ref $tuple) local.set $a
 local.get $list ref.cast (ref $tuple) i32.const 1 array.get $tuple call $index local.set $n
 block $done loop $count
 local.get $i local.get $n i32.ge_u br_if $done
 local.get $a local.get $i array.get $tuple ref.cast (ref $bytes) array.len i64.extend_i32_u local.get $size i64.add local.set $size
 local.get $i i32.const 1 i32.add local.set $i br $count end end
 local.get $size i64.const 4294967295 i64.gt_u if unreachable end
 local.get $size i32.wrap_i64 array.new_default $bytes local.set $r i32.const 0 local.set $i
 block $done loop $copy
 local.get $i local.get $n i32.ge_u br_if $done
 local.get $a local.get $i array.get $tuple ref.cast (ref $bytes) local.tee $s array.len local.set $len
 local.get $r local.get $pos local.get $s i32.const 0 local.get $len array.copy $bytes $bytes
 local.get $pos local.get $len i32.add local.set $pos
 local.get $i i32.const 1 i32.add local.set $i br $copy end end local.get $r)
(func $match_bytes (param $s (ref $bytes)) (param $i i32) (param $p (ref $bytes)) (result i32) (local $j i32)
 local.get $i local.get $s array.len i32.gt_u if i32.const 0 return end
 local.get $p array.len local.get $s array.len local.get $i i32.sub i32.gt_u if i32.const 0 return end
 block $done loop $scan
 local.get $j local.get $p array.len i32.ge_u br_if $done
 local.get $p local.get $j array.get_u $bytes local.get $s local.get $i local.get $j i32.add array.get_u $bytes i32.ne
 if i32.const 0 return end
 local.get $j i32.const 1 i32.add local.set $j br $scan end end i32.const 1)
(func $str_match (param $s (ref null eq)) (param $i (ref null eq)) (param $p (ref null eq)) (result (ref null eq)) (local $n i64)
 local.get $i call $unbox local.tee $n i64.const 4294967295 i64.gt_u if i32.const 0 ref.i31 return end
 local.get $s ref.cast (ref $bytes) local.get $n i32.wrap_i64 local.get $p ref.cast (ref $bytes) call $match_bytes ref.i31)
(func $contains (param $p (ref null eq)) (param $s (ref null eq)) (result (ref null eq)) (local $a (ref $bytes)) (local $b (ref $bytes)) (local $i i32)
 local.get $s ref.cast (ref $bytes) local.set $a local.get $p ref.cast (ref $bytes) local.set $b
 block $done loop $scan
 local.get $a local.get $i local.get $b call $match_bytes if i32.const 1 ref.i31 return end
 local.get $i local.get $a array.len i32.eq br_if $done
 local.get $i i32.const 1 i32.add local.set $i br $scan end end i32.const 0 ref.i31)
(func $json_string (param $s (ref null eq)) (param $start (ref null eq)) (result (ref null eq))
 (local $a (ref $bytes)) (local $r (ref $bytes)) (local $i i32) (local $begin i32) (local $end i32) (local $n i32) (local $c i32)
 local.get $s ref.cast (ref $bytes) local.set $a local.get $start call $index local.tee $begin local.set $i
 local.get $a local.get $i array.get_u $bytes i32.const 34 i32.ne if unreachable end
 local.get $i i32.const 1 i32.add local.set $i
 block $done loop $scan
 local.get $a local.get $i array.get_u $bytes local.tee $c i32.const 34 i32.eq br_if $done
 local.get $c i32.const 92 i32.eq if local.get $i i32.const 1 i32.add local.set $i end
 local.get $i i32.const 1 i32.add local.set $i local.get $n i32.const 1 i32.add local.set $n br $scan end end
 local.get $i local.set $end local.get $n array.new_default $bytes local.set $r
 local.get $begin i32.const 1 i32.add local.set $i i32.const 0 local.set $n
 block $done loop $decode
 local.get $i local.get $end i32.ge_u br_if $done
 local.get $a local.get $i array.get_u $bytes local.set $c
 local.get $c i32.const 92 i32.eq if
 local.get $i i32.const 1 i32.add local.set $i local.get $a local.get $i array.get_u $bytes local.set $c
 local.get $c i32.const 110 i32.eq if i32.const 10 local.set $c else
 local.get $c i32.const 116 i32.eq if i32.const 9 local.set $c else
 local.get $c i32.const 114 i32.eq if i32.const 13 local.set $c else
 local.get $c i32.const 98 i32.eq if i32.const 8 local.set $c else
 local.get $c i32.const 102 i32.eq if i32.const 12 local.set $c else
 local.get $c i32.const 92 i32.eq local.get $c i32.const 47 i32.eq i32.or local.get $c i32.const 34 i32.eq i32.or i32.eqz if unreachable end
 end end end end end end
 local.get $r local.get $n local.get $c array.set $bytes
 local.get $i i32.const 1 i32.add local.set $i local.get $n i32.const 1 i32.add local.set $n br $decode end end
 local.get $r local.get $end local.get $begin i32.sub i32.const 1 i32.add i64.extend_i32_u call $box array.new_fixed $tuple 2)
