-- 文言：版不改名，重上传则增序；旧籍旧物皆存。
-- 汉语：每个包版本独立编号修订，旧记录为第 1 次上传，原 ID 和 R2 路径不变。
CREATE TABLE "即时版本新" (
  "编号" TEXT PRIMARY KEY CHECK(length("编号")=32),
  "所有者编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
  "名称" TEXT NOT NULL,
  "版本" TEXT NOT NULL,
  "类型" TEXT NOT NULL CHECK("类型" IN ('库','可执行文件')),
  "简介" TEXT NOT NULL,
  "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  "归档摘要" TEXT,
  "上传序数" INTEGER NOT NULL DEFAULT 1 CHECK("上传序数">0),
  UNIQUE("所有者编号","名称","版本","上传序数"),
  UNIQUE("所有者编号","名称","版本","归档摘要")
);
INSERT INTO "即时版本新" SELECT *,1 FROM "即时版本";
DROP TABLE "即时版本";
ALTER TABLE "即时版本新" RENAME TO "即时版本";
