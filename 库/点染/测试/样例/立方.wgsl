struct 变换 {
  合矩: mat4x4<f32>,
  转矩: mat4x4<f32>,
  光向: vec4<f32>,
}

@group(0) @binding(0) var<uniform> 均匀: 变换;

struct 顶点入 {
  @location(0) 方位: vec3<f32>,
  @location(1) 法向: vec3<f32>,
  @location(2) 底色: vec3<f32>,
}

struct 顶点出 {
  @builtin(position) 方位: vec4<f32>,
  @location(0) 颜色: vec3<f32>,
}

@vertex
fn 立方顶点(入: 顶点入) -> 顶点出 {
  let 变 = 均匀;
  let 合 = 变.合矩;
  let 转 = 变.转矩;
  let 光 = 变.光向;
  let 坐 = 入.方位;
  let 法 = 入.法向;
  let 底 = 入.底色;
  let 点横 = 坐.x;
  let 点纵 = 坐.y;
  let 点深 = 坐.z;
  let 法横 = 法.x;
  let 法纵 = 法.y;
  let 法深 = 法.z;
  let 齐点 = vec4<f32>(点横, 点纵, 点深, 1.0f);
  let 齐法 = vec4<f32>(法横, 法纵, 法深, 0.0f);
  let 位 = 合 * 齐点;
  let 转法 = 转 * 齐法;
  let 漫 = max(dot(转法, 光), 0.0f);
  let 亮 = 0.3f + (0.7f * 漫);
  let 色 = 底 * 亮;
  return 顶点出(位, 色);
}

@fragment
fn 立方片元(入: 顶点出) -> @location(0) vec4<f32> {
  let 色 = 入.颜色;
  let 红 = 色.x;
  let 绿 = 色.y;
  let 蓝 = 色.z;
  return vec4<f32>(红, 绿, 蓝, 1.0f);
}
