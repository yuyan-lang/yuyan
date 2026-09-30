struct 计算输入 {
  @builtin(global_invocation_id) 全局: vec3<u32>,
  @builtin(local_invocation_index) 局部: u32,
}

struct 参数 {
  倍率: f32,
  上限: f32,
  个数: u32,
}

@group(0) @binding(0) var<uniform> 设置: 参数;

@group(0) @binding(1) var<storage, read> 输入: array<f32>;

@group(0) @binding(2) var<storage, read_write> 输出: array<f32>;

@group(0) @binding(3) var<storage, read_write> 计数: array<atomic<u32>>;

var<workgroup> 组内个数: u32;

fn 缩放一个(值: f32) -> f32 {
  let 参 = 设置;
  return min((值 * 参.倍率), 参.上限);
}

fn 处理(值: f32) -> f32 {
  let 缩 = 缩放一个(值);
  return (缩 % 1.0f) + round(缩);
}

fn 组内上限() -> u32 {
  return 组内个数;
}

@compute @workgroup_size(4)
fn 缩放(入: 计算输入) {
  let 序 = 入.全局.x;
  let 局部序 = 入.局部;
  let 参 = 设置;
  if (局部序 == 0u) {
    组内个数 = max(参.个数, 1u);
  } else {
  }
  workgroupBarrier();
  if (序 < 组内上限()) {
    输出[序] = 处理(输入[序]);
    _ = atomicAdd(&计数[0u], 1u);
  } else {
  }
}
