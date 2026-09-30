enable subgroups;

struct 输入 {
  @builtin(local_invocation_index) 局部: u32,
  @builtin(subgroup_invocation_id) 道: u32,
  @builtin(subgroup_size) 宽: u32,
}

@group(0) @binding(0) var<storage, read_write> 出: array<f32>;

@group(0) @binding(1) var<storage, read_write> 整出: array<u32>;

@compute @workgroup_size(48)
fn 子组试(入: 输入) {
  let 局部 = 入.局部;
  let 道 = 入.道;
  let 宽 = 入.宽;
  let 值 = f32(局部);
  let 和 = subgroupAdd(值);
  let 大 = subgroupMax(值);
  let 小 = subgroupMin(值);
  let 首 = subgroupShuffle(值, 0u);
  let 邻 = subgroupShuffle(值, (道 ^ 1u));
  let 异 = subgroupShuffleXor(值, 3u);
  let 整和 = subgroupAdd(局部);
  let 一 = (和 + 大) + 小;
  let 二 = (首 + 邻) + 异;
  出[局部] = 一 + 二;
  整出[局部] = 整和 + (道 + 宽);
}
