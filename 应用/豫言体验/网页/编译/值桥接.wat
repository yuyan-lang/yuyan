(module
 ;; 文言：客值不改，唯桥可读写。汉语：类型与编译器的结构类型相同，线性内存仅作宿主字节交换区。
 (type $big (struct (field i64)))
 (type $tuple (array (mut (ref null eq))))
 (type $float (struct (field f64)))
 (type $bytes (array (mut i8)))
 (memory (export "memory") 1)
 (func (export "kind") (param $v (ref null eq)) (result i32)
  local.get $v ref.is_null if i32.const 0 return end
  local.get $v ref.test (ref i31) if i32.const 1 return end
  local.get $v ref.test (ref $bytes) if i32.const 2 return end
  local.get $v ref.test (ref $tuple) if i32.const 3 return end
  local.get $v ref.test (ref $big) if i32.const 4 return end
  local.get $v ref.test (ref $float) if i32.const 5 return end
  unreachable)
 (func (export "int") (param $v (ref null eq)) (result i64)
  local.get $v ref.test (ref i31) if (result i64)
   local.get $v ref.cast (ref i31) i31.get_s i64.extend_i32_s
  else local.get $v ref.cast (ref $big) struct.get $big 0 end)
 (func (export "new_int") (param $v i64) (result (ref null eq))
  local.get $v i64.const -1073741824 i64.ge_s local.get $v i64.const 1073741823 i64.le_s i32.and
  if (result (ref null eq)) local.get $v i32.wrap_i64 ref.i31 else local.get $v struct.new $big end)
 (func (export "float") (param $v (ref null eq)) (result f64) local.get $v ref.cast (ref $float) struct.get $float 0)
 (func (export "new_float") (param $v f64) (result (ref $float)) local.get $v struct.new $float)
 (func (export "tuple_len") (param $v (ref null eq)) (result i32) local.get $v ref.cast (ref $tuple) array.len)
 (func (export "tuple_get") (param $v (ref null eq)) (param $i i32) (result (ref null eq)) local.get $v ref.cast (ref $tuple) local.get $i array.get $tuple)
 (func (export "tuple_set") (param $v (ref null eq)) (param $i i32) (param $x (ref null eq)) local.get $v ref.cast (ref $tuple) local.get $i local.get $x array.set $tuple)
 (func (export "new_tuple") (param $n i32) (result (ref $tuple)) local.get $n array.new_default $tuple)
 (func (export "bytes_len") (param $v (ref null eq)) (result i32) local.get $v ref.cast (ref $bytes) array.len)
 (func (export "bytes_out") (param $v (ref null eq)) (local $a (ref $bytes)) (local $n i32) (local $i i32)
  local.get $v ref.cast (ref $bytes) local.tee $a array.len local.set $n
  block $done loop $loop local.get $i local.get $n i32.ge_u br_if $done
   local.get $i local.get $a local.get $i array.get_u $bytes i32.store8
   local.get $i i32.const 1 i32.add local.set $i br $loop end end)
 (func (export "bytes_in") (param $n i32) (result (ref $bytes)) (local $a (ref $bytes)) (local $i i32)
  local.get $n array.new_default $bytes local.set $a
  block $done loop $loop local.get $i local.get $n i32.ge_u br_if $done
   local.get $a local.get $i local.get $i i32.load8_u array.set $bytes
   local.get $i i32.const 1 i32.add local.set $i br $loop end end local.get $a)
)
