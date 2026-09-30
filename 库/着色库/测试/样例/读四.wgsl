@group(0) @binding(0) var<storage, read> 浮源: array<f32>;

@group(0) @binding(1) var<storage, read> 整源: array<u32>;

@group(0) @binding(2) var<storage, read_write> 出: array<f32>;

@group(0) @binding(3) var<storage, read_write> 整出: array<u32>;

@compute @workgroup_size(8)
fn 读四试(@builtin(local_invocation_index) 局部: u32) {
  let 起 = 局部 * 4u;
  let 浮四 = vec4<f32>(浮源[起], 浮源[(起) + 1u], 浮源[(起) + 2u], 浮源[(起) + 3u]);
  let 整四 = vec4<u32>(整源[起], 整源[(起) + 1u], 整源[(起) + 2u], 整源[(起) + 3u]);
  let 浮零 = 浮四.x;
  let 浮一 = 浮四.y;
  let 浮二 = 浮四.z;
  let 浮三 = 浮四.w;
  let 整零 = 整四.x;
  let 整一 = 整四.y;
  let 整二 = 整四.z;
  let 整三 = 整四.w;
  let 浮前 = 浮零 + 浮一;
  let 浮后 = 浮二 + 浮三;
  let 整前 = 整零 + 整一;
  let 整后 = 整二 + 整三;
  出[局部] = 浮前 + 浮后;
  整出[局部] = 整前 + 整后;
}
