# ONNX 外部数据

`ONNX加载外部数据` 接收已授权模型目录与 TensorProto 字段消息，通过文件接口实际读取 `location` 文件，按 `offset` 和 `length` 分段定位读取，返回含 `raw_data` 的内部张量消息。省略 offset 时从零开始，省略 length 时读取到末尾。

`ONNX写出外部数据` 接收已授权可写目录、相对输出路径和含一个 raw_data 字段的张量，分段写入实际文件后返回 location、offset、length 与 EXTERNAL 标志。文件接口执行目录边界检查。应用在关闭文件后继续处理返回消息。

基本 CI 测试覆盖元数据范围、默认值及非法负数拒绝。实际系统文件读写和跨平台运行验收列为后续事项。
