@group(0) @binding(0) var<storage, read> 输入: array<u32>;

@group(0) @binding(1) var<storage, read_write> 输出: array<u32>;

fn 前缀和(个数: u32) -> u32 {
  var 次: u32 = 0u;
  var 和: u32 = 0u;
  loop {
    if (次 > 个数) {
      return 和;
    } else {
      let 临时 = 次 + 1u;
      let 临时_2 = 和 + 输入[次];
      次 = 临时;
      和 = 临时_2;
      continue;
    }
  }
}

fn 较大(甲: u32, 乙: u32) -> u32 {
  return select(乙, 甲, (甲 > 乙));
}

@compute @workgroup_size(64)
fn 求和(@builtin(global_invocation_id) 号: vec3<u32>) {
  let 序 = 号.x;
  var 计数: u32 = 0u;
  if (序 > 0u) {
    计数 = 前缀和(序);
    输出[0u] = 1u;
  } else {
    计数 = 较大(输入[0u], 7u);
  }
  输出[序] = 计数;
}
