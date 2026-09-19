(func $f786 (type $t2)
(param $a0 (ref null eq))
(param $a1 (ref null eq))
(result (ref null eq)) (local $u (ref null eq))
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
(local $a22 (ref null eq))
(local $a23 (ref null eq))
(local $a24 (ref null eq))
(local $a25 (ref null eq))
(local $a26 (ref null eq))
(local $a27 (ref null eq))
(local $a28 (ref null eq))
(local $a29 (ref null eq))
(local $a30 (ref null eq))
(local $a31 (ref null eq))
(local $a32 (ref null eq))
(local $a33 (ref null eq))
(local $a34 (ref null eq))
(local $a35 (ref null eq))
(local $a36 (ref null eq))
(local $a37 (ref null eq))
(local $a38 (ref null eq))
(local $a39 (ref null eq))
(local $a40 (ref null eq))
(local $a41 (ref null eq))
(local $a42 (ref null eq))
(local $a43 (ref null eq))
(local $a44 (ref null eq))
(local $a45 (ref null eq))
(local $a46 (ref null eq))
(local $a47 (ref null eq))
(local $a48 (ref null eq))
local.get $a0

ref.cast (ref $tuple) i32.const 1 array.get $tuple

local.set $a2
local.get $a0

ref.cast (ref $tuple) i32.const 2 array.get $tuple

local.set $a3
local.get $a2

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.get $a3

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.eq
ref.i31
local.set $a4
local.get $a4

ref.cast (ref i31) i31.get_u if (result (ref null eq))
local.get $a1

else
local.get $a0

ref.cast (ref $tuple) i32.const 3 array.get $tuple

local.set $a5
local.get $a0

ref.cast (ref $tuple) i32.const 2 array.get $tuple

local.set $a6
local.get $a5

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.get $a6

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.mul
call $box
local.set $a7
i32.const 784 ref.i31

local.get $a7

array.new_fixed $tuple 2

local.set $a8
local.get $a8

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a9
local.get $a0

ref.cast (ref $tuple) i32.const 1 array.get $tuple

local.set $a10
local.get $a8

local.get $a10

local.get $a9

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a11
local.get $a0

ref.cast (ref $tuple) i32.const 4 array.get $tuple

local.set $a12
local.get $a12

local.get $a11

call $get

local.set $a13
local.get $a0

ref.cast (ref $tuple) i32.const 1 array.get $tuple

local.set $a14
local.get $a0

ref.cast (ref $tuple) i32.const 2 array.get $tuple

local.set $a15
local.get $a14

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.get $a15

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.mul
call $box
local.set $a16
i32.const 785 ref.i31

local.get $a16

array.new_fixed $tuple 2

local.set $a17
local.get $a17

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a18
local.get $a0

ref.cast (ref $tuple) i32.const 5 array.get $tuple

local.set $a19
local.get $a17

local.get $a19

local.get $a18

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a20
local.get $a0

ref.cast (ref $tuple) i32.const 6 array.get $tuple

local.set $a21
local.get $a21

local.get $a20

call $get

local.set $a22
local.get $a0

ref.cast (ref $tuple) i32.const 7 array.get $tuple

local.set $a23
local.get $a23

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a24
local.get $a24

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a25
local.get $a0

ref.cast (ref $tuple) i32.const 4 array.get $tuple

local.set $a26
local.get $a24

local.get $a26

local.get $a25

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a27
local.get $a27

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a28
local.get $a0

ref.cast (ref $tuple) i32.const 6 array.get $tuple

local.set $a29
local.get $a27

local.get $a29

local.get $a28

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a30
local.get $a30

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a31
local.get $a0

ref.cast (ref $tuple) i32.const 2 array.get $tuple

local.set $a32
local.get $a30

local.get $a32

local.get $a31

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a33
local.get $a33

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a34
local.get $a0

ref.cast (ref $tuple) i32.const 3 array.get $tuple

local.set $a35
local.get $a33

local.get $a35

local.get $a34

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a36
local.get $a36

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a37
local.get $a0

ref.cast (ref $tuple) i32.const 5 array.get $tuple

local.set $a38
local.get $a36

local.get $a38

local.get $a37

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a39
local.get $a39

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a40
local.get $a0

ref.cast (ref $tuple) i32.const 1 array.get $tuple

local.set $a41
local.get $a41

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i32.const 1 ref.i31

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.add
call $box
local.set $a42
local.get $a39

local.get $a42

local.get $a40

ref.cast (ref i31) i31.get_u
call_indirect (type $t2)
local.set $a43
local.get $a43

ref.cast (ref $tuple) i32.const 0 array.get $tuple

local.set $a44
local.get $a13

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.get $a22

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.mul
call $box
local.set $a45
local.get $a1

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
local.get $a45

local.tee $u ref.test (ref i31) if (result i64) local.get $u ref.cast (ref i31) i31.get_s i64.extend_i32_s else local.get $u ref.cast (ref $big) struct.get $big 0 end
i64.add
call $box
local.set $a46
local.get $a43

local.get $a46

local.get $a44

ref.cast (ref i31) i31.get_u
return_call_indirect (type $t2)
end return
)
