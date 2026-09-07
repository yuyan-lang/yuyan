-- 古曰：先立其籍，诸物各至；缺物不掩其页。
-- 今释：新接口独立保存所有者命名空间中的版本，旧 ZIP 版本和下载接口不变。
CREATE TABLE "即时版本" (
  "编号" TEXT PRIMARY KEY CHECK(length("编号")=32),
  "所有者编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
  "名称" TEXT NOT NULL,
  "版本" TEXT NOT NULL,
  "类型" TEXT NOT NULL CHECK("类型" IN ('库','可执行文件')),
  "简介" TEXT NOT NULL,
  "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE("所有者编号","名称","版本")
);
