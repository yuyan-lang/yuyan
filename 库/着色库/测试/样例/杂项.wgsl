@group(0) @binding(0) var 图: texture_2d<f32>;

@group(0) @binding(1) var 采样: sampler;

@group(0) @binding(2) var<storage, read_write> 计数: array<atomic<u32>>;

struct 调色 {
  颜色们: array<vec4<f32>, 4>,
}

@group(0) @binding(3) var<uniform> 调色板: 调色;

var<workgroup> 共享: u32;

var<workgroup> 分组: array<u32, 64>;

const 第一列: vec4<f32> = vec4<f32>(1.0f, 0.0f, 0.0f, 0.0f);

const 第二列: vec4<f32> = vec4<f32>(0.0f, 1.0f, 0.0f, 0.0f);

const 第三列: vec4<f32> = vec4<f32>(0.0f, 0.0f, 1.0f, 0.0f);

const 第四列: vec4<f32> = vec4<f32>(0.0f, 0.0f, 0.0f, 1.0f);

const 变换: mat4x4<f32> = mat4x4<f32>(第一列, 第二列, 第三列, 第四列);

@fragment
fn 着色(@builtin(position) 坐标: vec4<f32>) -> @location(0) vec4<f32> {
  let 尺寸 = vec2<f32>(textureDimensions(图));
  let 横 = 坐标.x;
  let 纵 = 坐标.y;
  let 比 = vec2<f32>((横 / 尺寸.x), (纵 / 尺寸.y));
  let 色 = textureSample(图, 采样, 比);
  return 变换 * 色;
}

@compute @workgroup_size(64)
fn 统计(@builtin(local_invocation_index) 序: u32) {
  共享 = 序;
  分组[序] = 序 * 2u;
  workgroupBarrier();
  let 旧 = atomicAdd(&计数[0u], 共享);
  let 颜色们 = 调色板.颜色们;
  let 亮度 = 颜色们[1u].x;
  let 位 = bitcast<u32>((f32(旧) + 亮度));
  atomicStore(&计数[1u], (位 + 分组[0u]));
}
