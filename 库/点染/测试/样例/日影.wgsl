struct 变换 {
  视投影: mat4x4<f32>,
  光投影: mat4x4<f32>,
  光向: vec4<f32>,
}

@group(0) @binding(0) var<uniform> 均匀: 变换;

@group(0) @binding(1) var 高图: texture_2d<f32>;

@group(0) @binding(2) var 高采: sampler;

@group(0) @binding(3) var 影图: texture_depth_2d;

@group(0) @binding(4) var 影采: sampler_comparison;

struct 顶点入 {
  @location(0) 方位: vec3<f32>,
  @location(1) 纹坐: vec2<f32>,
  @location(2) 偏移: vec3<f32>,
  @builtin(instance_index) 实例: u32,
}

struct 顶点出 {
  @builtin(position) 方位: vec4<f32>,
  @location(0) 世界: vec3<f32>,
  @location(1) 光位: vec4<f32>,
  @location(2) 纹坐: vec2<f32>,
}

@vertex
fn 影顶点(入: 顶点入) -> 顶点出 {
  let 变 = 均匀;
  let 视投影 = 变.视投影;
  let 光投影 = 变.光投影;
  let 坐 = 入.纹坐;
  let 零层 = 0.0f;
  let 高色 = textureSampleLevel(高图, 高采, 坐, 零层);
  let 抬 = 高色.x;
  let 方 = 入.方位;
  let 移 = 入.偏移;
  let 序 = 入.实例;
  let 层 = f32(序);
  let 方横 = 方.x;
  let 方纵 = 方.y;
  let 方深 = 方.z;
  let 移横 = 移.x;
  let 移纵 = 移.y;
  let 移深 = 移.z;
  let 横 = 方横 + 移横;
  let 纵和 = 方纵 + 移纵;
  let 层抬 = 抬 * 层;
  let 纵 = 纵和 + 层抬;
  let 深 = 方深 + 移深;
  let 世 = vec3<f32>(横, 纵, 深);
  let 齐 = vec4<f32>(横, 纵, 深, 1.0f);
  let 位 = 视投影 * 齐;
  let 光 = 光投影 * 齐;
  return 顶点出(位, 世, 光, 坐);
}

@fragment
fn 影片元(入: 顶点出) -> @location(0) vec4<f32> {
  let 变 = 均匀;
  let 光向 = 变.光向;
  let 光位 = 入.光位;
  let 光宽 = 光位.w;
  let 齐横 = 光位.x;
  let 齐纵 = 光位.y;
  let 齐深 = 光位.z;
  let 光横 = 齐横 / 光宽;
  let 光纵 = 齐纵 / 光宽;
  let 光深 = 齐深 / 光宽;
  let 半 = 0.5f;
  let 横半 = 光横 * 半;
  let 纵半 = 光纵 * 半;
  let 影横 = 横半 + 半;
  let 影纵 = 半 - 纵半;
  let 影坐 = vec2<f32>(影横, 影纵);
  let 参照 = 光深 - 0.002f;
  let 受光 = textureSampleCompare(影图, 影采, 影坐, 参照);
  let 世 = 入.世界;
  let 横变 = dpdx(世);
  let 纵变 = dpdy(世);
  let 叉 = cross(横变, 纵变);
  let 法 = normalize(叉);
  let 光甲 = 光向.x;
  let 光乙 = 光向.y;
  let 光丙 = 光向.z;
  let 光三 = vec3<f32>(光甲, 光乙, 光丙);
  let 漫 = max(dot(法, 光三), 0.0f);
  let 照 = 受光 * 漫;
  let 亮 = 0.25f + 照;
  let 坐 = 入.纹坐;
  let 底 = textureSample(高图, 高采, 坐);
  let 底红 = 底.x;
  let 底绿 = 底.y;
  let 底蓝 = 底.z;
  let 红 = 底红 * 亮;
  let 绿 = 底绿 * 亮;
  let 蓝 = 底蓝 * 亮;
  return vec4<f32>(红, 绿, 蓝, 1.0f);
}
