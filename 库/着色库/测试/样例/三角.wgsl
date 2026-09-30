struct 顶点输出 {
  @builtin(position) 位置: vec4<f32>,
  @location(0) 颜色: vec4<f32>,
  @location(1) @interpolate(flat) 层: u32,
}

@vertex
fn 顶点主(@location(0) 坐标: vec2<f32>) -> 顶点输出 {
  return 顶点输出(vec4<f32>(坐标.x, 坐标.y, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0), 0);
}

@fragment
fn 片元主(输入: 顶点输出) -> @location(0) vec4<f32> {
  return 输入.颜色;
}
