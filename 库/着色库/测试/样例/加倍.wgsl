@group(0) @binding(0) var<storage, read_write> 数据: array<u32>;

@compute @workgroup_size(4)
fn 加倍(@builtin(global_invocation_id) 号: vec3<u32>) {
  let 序 = 号.x;
  数据[序] = 数据[序] * 2;
}
