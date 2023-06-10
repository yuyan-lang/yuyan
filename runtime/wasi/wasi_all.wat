
(module
  ;; Import the necessary WASI functions
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i64 i64 i64 i64) (result i64)))
  (import "wasi_snapshot_preview1" "environ_sizes_get" (func $environ_sizes_get (param i64 i64) (result i64)))
  (import "wasi_snapshot_preview1" "environ_get" (func $environ_get (param i64 i64) (result i64)))
  (memory (export "memory") 1 2)
  ;; ...

  (global $offset (mut i64) (i64.const 0))
  ;; ...
  (func $yy_gcAllocateBytes (param $size i64) (result i64)
    ;; Get the current memory offset
    (local $currentOffset i64)
    (local $newOffset i64)
    (local $memorySize i64)
    (local.set $currentOffset (global.get $offset))

    ;; Calculate the new memory offset
    (local.set $newOffset (i64.add (local.get $currentOffset) (local.get $size)))

    ;; Check if the new offset exceeds the current memory size
    (local.set $memorySize (i64.mul (i64.extend_i32_u (memory.size)) (i64.const 65536)))
    (if (result i64) (i64.gt_u (local.get $memorySize) (local.get $newOffset))
      ;; Memory allocation successful, update the current offset and return the previous offset
      (then
        (global.set $offset (local.get $newOffset))
        (local.get $currentOffset)
      )
      ;; Memory allocation failed, return -1
      (else
        (i64.const -1)
      )
    )
  )
  ;; ...
   (func $yy_gcAllocateArray (param $numElements i64) (result i64)
    ;; Calculate the total size required for the array
    (local.get $numElements)
    (i64.const 8)  ;; Element size (i64) in bytes
    (i64.mul)

    ;; Call the yy_gcAllocateBytes function with the calculated size
    (call $yy_gcAllocateBytes)
  )
)

