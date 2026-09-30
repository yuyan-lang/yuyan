struct 参数 {
  缩放: f32,
  偏移: vec2<f32>,
}

@group(0) @binding(0) var<uniform> 均匀: 参数;

struct 顶点输入 {
  @builtin(vertex_index) 序号: u32,
  @builtin(instance_index) 实例: u32,
}

fn 角坐标(序: u32) -> vec2<f32> {
  let 横 = select((-1.0f), 3.0f, (序 == 1u));
  let 纵 = select((-1.0f), 3.0f, (序 == 2u));
  return vec2<f32>(横, 纵);
}

@vertex
fn 全屏顶点(入: 顶点输入) -> @builtin(position) vec4<f32> {
  let 参 = 均匀;
  let 缩放 = 参.缩放;
  let 偏移 = 参.偏移;
  let 角 = 角坐标(入.序号);
  let 位 = (角 * 缩放) + 偏移;
  return vec4<f32>(位.x, 位.y, 0.0f, 1.0f);
}

fn 分段(甲: f32) -> f32 {
  var 临时: f32;
  if (甲 < 0.5f) {
    let 丙 = 甲 * 2.0f;
    临时 = 丙 * 丙;
  } else {
    临时 = 1.0f;
  }
  let 乙 = 临时;
  return 乙 + 0.1f;
}

@fragment
fn 着色片元(@builtin(position) 坐标: vec4<f32>) -> @location(0) vec4<f32> {
  let 亮 = 分段(fract((坐标.x * 0.01f)));
  return vec4<f32>(亮, 亮, 亮, 1.0f);
}
