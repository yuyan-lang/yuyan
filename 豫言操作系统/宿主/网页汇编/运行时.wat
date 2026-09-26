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
;; 文言：整数自书于客堆，不出宿主；负极以无符号绝值除之。
;; 汉语：完整 i64 的十进制转换在 Wasm 内完成，只分配一次结果；无符号取商兼容 INT64_MIN。
(func $int_string (param $v (ref null eq)) (result (ref null eq))
 (local $n i64) (local $rest i64) (local $negative i32)
 (local $length i32) (local $position i32) (local $out (ref $bytes))
 local.get $v call $unbox local.tee $n i64.const 0 i64.lt_s local.set $negative
 local.get $negative if i64.const 0 local.get $n i64.sub local.set $n end
 local.get $n local.set $rest
 i32.const 1 local.set $length
 block $counted
  loop $count
   local.get $rest i64.const 10 i64.lt_u br_if $counted
   local.get $rest i64.const 10 i64.div_u local.set $rest
   local.get $length i32.const 1 i32.add local.set $length
   br $count
  end
 end
 local.get $length local.get $negative i32.add local.tee $position array.new_default $bytes local.set $out
 loop $digits
  local.get $position i32.const 1 i32.sub local.set $position
  local.get $out local.get $position
  local.get $n i64.const 10 i64.rem_u i32.wrap_i64 i32.const 48 i32.add array.set $bytes
  local.get $n i64.const 10 i64.div_u local.tee $n i64.const 0 i64.ne br_if $digits
 end
 local.get $negative if local.get $out i32.const 0 i32.const 45 array.set $bytes end
 local.get $out)
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
;; 文言：JSON 串之转义，今识 \uXXXX；代理成对合为一码，孤者代以 U+FFFD；余者非法即止。
;; 汉语：JSON 字符串解码支持 \n \t \r \b \f \\ \/ \" 与 \uXXXX（四位十六进制，大小写均可）。\uXXXX 转成 UTF-8：高代理项后紧随低代理项则合并为一个码点（四字节）；孤立的高/低代理项替换为 U+FFFD（EF BF BD）；\u0000 产生字节 0（字节数组自带长度，不当作串尾）。\u 后不足四位或含非十六进制字符、其他未知转义字符一律 unreachable（与旧行为一致）。
(func $json_hex_digit (param $c i32) (result i32)
 local.get $c i32.const 48 i32.sub i32.const 10 i32.lt_u if local.get $c i32.const 48 i32.sub return end
 local.get $c i32.const 65 i32.sub i32.const 6 i32.lt_u if local.get $c i32.const 55 i32.sub return end
 local.get $c i32.const 97 i32.sub i32.const 6 i32.lt_u if local.get $c i32.const 87 i32.sub return end
 unreachable)
(func $json_hex4 (param $a (ref $bytes)) (param $p i32) (result i32) (local $v i32) (local $k i32)
 block $done loop $next
 local.get $k i32.const 4 i32.ge_u br_if $done
 local.get $v i32.const 4 i32.shl local.get $a local.get $p local.get $k i32.add array.get_u $bytes call $json_hex_digit i32.or local.set $v
 local.get $k i32.const 1 i32.add local.set $k br $next end end
 local.get $v)
;; 文言：自反斜杠解一转义，返「码点左移四位，并其源长」；两遍同用此术，故长短必合。
;; 汉语：a[i] 为反斜杠。返回 (码点 << 4) | 源字节数；源字节数为 2（短转义）、6（\uXXXX，含孤立代理项→U+FFFD）或 12（代理对）。第一遍算长度与第二遍写出共用本函数，两遍推进的步长必然一致。高代理项后的 \u 若接非十六进制字符，此处即中止，与下一轮再中止等价。
(func $json_escape (param $a (ref $bytes)) (param $i i32) (result i32)
 (local $c i32) (local $u i32) (local $v i32) (local $len i32)
 local.get $a local.get $i i32.const 1 i32.add array.get_u $bytes local.set $c
 i32.const 2 local.set $len
 local.get $c i32.const 110 i32.eq if i32.const 10 local.set $u else
 local.get $c i32.const 116 i32.eq if i32.const 9 local.set $u else
 local.get $c i32.const 114 i32.eq if i32.const 13 local.set $u else
 local.get $c i32.const 98 i32.eq if i32.const 8 local.set $u else
 local.get $c i32.const 102 i32.eq if i32.const 12 local.set $u else
 local.get $c i32.const 117 i32.eq if
 i32.const 6 local.set $len
 local.get $a local.get $i i32.const 2 i32.add call $json_hex4 local.set $u
 local.get $u i32.const 63488 i32.and i32.const 55296 i32.eq if
 block $paired
 block $lone
 local.get $u i32.const 56320 i32.ge_u br_if $lone
 local.get $i i32.const 11 i32.add local.get $a array.len i32.ge_u br_if $lone
 local.get $a local.get $i i32.const 6 i32.add array.get_u $bytes i32.const 92 i32.ne br_if $lone
 local.get $a local.get $i i32.const 7 i32.add array.get_u $bytes i32.const 117 i32.ne br_if $lone
 local.get $a local.get $i i32.const 8 i32.add call $json_hex4 local.tee $v i32.const 64512 i32.and i32.const 56320 i32.ne br_if $lone
 local.get $u i32.const 55296 i32.sub i32.const 10 i32.shl local.get $v i32.const 56320 i32.sub i32.add i32.const 65536 i32.add local.set $u
 i32.const 12 local.set $len
 br $paired
 end
 i32.const 65533 local.set $u
 end
 end
 else
 local.get $c i32.const 92 i32.eq local.get $c i32.const 47 i32.eq i32.or local.get $c i32.const 34 i32.eq i32.or i32.eqz if unreachable end
 local.get $c local.set $u
 end end end end end end
 local.get $u i32.const 4 i32.shl local.get $len i32.or)
;; 文言：码点之 UTF-8 长与写。汉语：码点占 1–4 字节；json_utf8_put 写入 r[n..] 并返回新的写入位。
(func $json_utf8_size (param $c i32) (result i32)
 i32.const 1 local.get $c i32.const 128 i32.ge_u i32.add local.get $c i32.const 2048 i32.ge_u i32.add local.get $c i32.const 65536 i32.ge_u i32.add)
(func $json_utf8_put (param $r (ref $bytes)) (param $n i32) (param $c i32) (result i32)
 local.get $c i32.const 128 i32.lt_u if
 local.get $r local.get $n local.get $c array.set $bytes
 local.get $n i32.const 1 i32.add return end
 local.get $c i32.const 2048 i32.lt_u if
 local.get $r local.get $n local.get $c i32.const 6 i32.shr_u i32.const 192 i32.or array.set $bytes
 local.get $r local.get $n i32.const 1 i32.add local.get $c i32.const 63 i32.and i32.const 128 i32.or array.set $bytes
 local.get $n i32.const 2 i32.add return end
 local.get $c i32.const 65536 i32.lt_u if
 local.get $r local.get $n local.get $c i32.const 12 i32.shr_u i32.const 224 i32.or array.set $bytes
 local.get $r local.get $n i32.const 1 i32.add local.get $c i32.const 6 i32.shr_u i32.const 63 i32.and i32.const 128 i32.or array.set $bytes
 local.get $r local.get $n i32.const 2 i32.add local.get $c i32.const 63 i32.and i32.const 128 i32.or array.set $bytes
 local.get $n i32.const 3 i32.add return end
 local.get $r local.get $n local.get $c i32.const 18 i32.shr_u i32.const 240 i32.or array.set $bytes
 local.get $r local.get $n i32.const 1 i32.add local.get $c i32.const 12 i32.shr_u i32.const 63 i32.and i32.const 128 i32.or array.set $bytes
 local.get $r local.get $n i32.const 2 i32.add local.get $c i32.const 6 i32.shr_u i32.const 63 i32.and i32.const 128 i32.or array.set $bytes
 local.get $r local.get $n i32.const 3 i32.add local.get $c i32.const 63 i32.and i32.const 128 i32.or array.set $bytes
 local.get $n i32.const 4 i32.add)
;; 文言：明文一段，直至引号或反斜杠而止；无呼叫之紧环，乃两遍之所共。
;; 汉语：返回 a 中自 i 起第一个 " 或 \ 的下标（无结束引号即数组越界陷阱）。环内不含调用，保持热路径紧凑。
(func $json_plain_end (param $a (ref $bytes)) (param $i i32) (result i32) (local $c i32)
 block $done loop $next
 local.get $a local.get $i array.get_u $bytes local.tee $c i32.const 34 i32.eq br_if $done
 local.get $c i32.const 92 i32.eq br_if $done
 local.get $i i32.const 1 i32.add local.set $i br $next end end
 local.get $i)
;; 文言：先量后书，两遍同径；返回解码字节与所耗源字节（含首尾引号）。
;; 汉语：start 须指向引号。第一遍跨过明文段、逐个解转义，累计输出字节数并找到结束引号；无转义（每个转义都缩短输出，故输出长等于源长当且仅当无转义）则整段 array.copy；否则第二遍明文段整段 array.copy，转义段写出其 UTF-8。无转义的字节原样复制（不校验 UTF-8，与旧行为一致）。单字节码点（e < 2048，即码点 < 128）免调用 utf8 术。
(func $json_string (param $s (ref null eq)) (param $start (ref null eq)) (result (ref null eq))
 (local $a (ref $bytes)) (local $r (ref $bytes)) (local $i i32) (local $begin i32) (local $end i32) (local $n i32) (local $e i32) (local $j i32)
 local.get $s ref.cast (ref $bytes) local.set $a local.get $start call $index local.tee $begin local.set $i
 local.get $a local.get $i array.get_u $bytes i32.const 34 i32.ne if unreachable end
 local.get $i i32.const 1 i32.add local.set $i
 block $done loop $scan
 local.get $a local.get $i call $json_plain_end local.tee $j local.get $i i32.sub local.get $n i32.add local.set $n
 local.get $j local.set $i
 local.get $a local.get $i array.get_u $bytes i32.const 34 i32.eq br_if $done
 local.get $a local.get $i call $json_escape local.tee $e i32.const 15 i32.and local.get $i i32.add local.set $i
 local.get $e i32.const 2048 i32.lt_u if local.get $n i32.const 1 i32.add local.set $n else
 local.get $e i32.const 4 i32.shr_u call $json_utf8_size local.get $n i32.add local.set $n end
 br $scan end end
 local.get $i local.set $end local.get $n array.new_default $bytes local.set $r
 local.get $n local.get $end local.get $begin i32.sub i32.const 1 i32.sub i32.eq
 if
 local.get $r i32.const 0 local.get $a local.get $begin i32.const 1 i32.add local.get $n array.copy $bytes $bytes
 else
 local.get $begin i32.const 1 i32.add local.set $i i32.const 0 local.set $n
 block $done loop $decode
 local.get $a local.get $i call $json_plain_end local.tee $j local.get $i i32.sub local.tee $e
 if local.get $r local.get $n local.get $a local.get $i local.get $e array.copy $bytes $bytes
 local.get $n local.get $e i32.add local.set $n end
 local.get $j local.set $i
 local.get $i local.get $end i32.ge_u br_if $done
 local.get $a local.get $i call $json_escape local.tee $e i32.const 15 i32.and local.get $i i32.add local.set $i
 local.get $e i32.const 2048 i32.lt_u if
 local.get $r local.get $n local.get $e i32.const 4 i32.shr_u array.set $bytes local.get $n i32.const 1 i32.add local.set $n else
 local.get $r local.get $n local.get $e i32.const 4 i32.shr_u call $json_utf8_put local.set $n end
 br $decode end end
 end
 local.get $r local.get $end local.get $begin i32.sub i32.const 1 i32.add i64.extend_i32_u call $box array.new_fixed $tuple 2)
