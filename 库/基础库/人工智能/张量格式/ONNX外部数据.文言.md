# ONNX 外部数据

`ONNX加载外部数据` 受已授模型之目与 TensorProto 字段消息，经文件接口实读 `location` 之文，依 `offset`、`length` 分段定位而读，还含 `raw_data` 之内存张量消息。偏省则自零始，长省则读至末。

`ONNX写出外部数据` 受已授可写之目、相对输出径及具一个 raw_data 字段之张量，分段实写文件，复还 location、offset、length 与 EXTERNAL 标识。文件接口守目录之界。闭文而后应用续治所还消息。

基本 CI 之试核元数据范围、默认值与负数之拒。系统文件实读写及诸平台运行之验为后续之事。
