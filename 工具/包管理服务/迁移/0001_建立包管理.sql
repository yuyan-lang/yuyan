-- 古曰：用者惟豫言，令惟一符；名版皆具，既颁不篡。
-- 今释：首期只有“豫言”用户和一枚上传令牌；包名与数字版本不作特定语言和版本线限制，已发布版本不可覆盖。

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS "用户" (
    "编号" INTEGER PRIMARY KEY,
    "名称" TEXT NOT NULL UNIQUE,
    "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CHECK ("名称" = '豫言')
);

CREATE TABLE IF NOT EXISTS "访问令牌" (
    "编号" INTEGER PRIMARY KEY,
    "用户编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
    "名称" TEXT NOT NULL,
    "SHA256" TEXT NOT NULL UNIQUE,
    "可上传" INTEGER NOT NULL DEFAULT 1 CHECK ("可上传" IN (0, 1)),
    "已启用" INTEGER NOT NULL DEFAULT 1 CHECK ("已启用" IN (0, 1)),
    "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "最后使用时间" TEXT
);

CREATE TABLE IF NOT EXISTS "包" (
    "名称" TEXT PRIMARY KEY,
    "所有者编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
    "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS "上传" (
    "包名" TEXT NOT NULL,
    "版本" TEXT NOT NULL,
    "文件名" TEXT NOT NULL,
    "SHA256" TEXT NOT NULL,
    "预期字节数" INTEGER NOT NULL CHECK ("预期字节数" > 0),
    "待发布对象键" TEXT NOT NULL,
    "发布者编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
    "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("包名", "版本")
);

CREATE TABLE IF NOT EXISTS "包版本" (
    "包名" TEXT NOT NULL REFERENCES "包"("名称"),
    "版本" TEXT NOT NULL,
    "文件名" TEXT NOT NULL UNIQUE,
    "对象键" TEXT NOT NULL,
    "SHA256" TEXT NOT NULL,
    "压缩字节数" INTEGER NOT NULL CHECK ("压缩字节数" > 0),
    "文件数" INTEGER NOT NULL CHECK ("文件数" > 0),
    "解压字节数" INTEGER NOT NULL CHECK ("解压字节数" >= 0),
    "发布者编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
    "发布时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("包名", "版本")
);

CREATE INDEX IF NOT EXISTS "包版本_包名_版本"
    ON "包版本"("包名", "版本");

INSERT OR IGNORE INTO "用户" ("编号", "名称") VALUES (1, '豫言');

-- 明文令牌不入库；交付时只向管理者显示一次。
INSERT OR IGNORE INTO "访问令牌"
    ("编号", "用户编号", "名称", "SHA256", "可上传", "已启用")
VALUES
    (1, 1, '初始上传令牌', '0a4a58ed646751c03edc300b30abf54199e0c993bbed7712aeba84f77b558410', 1, 1);
