-- 古曰：去独户之限，旧籍仍存。今释：保留原用户、包与令牌，允许第三方邮箱账户。
PRAGMA defer_foreign_keys = ON;
CREATE TABLE "用户新" (
  "编号" INTEGER PRIMARY KEY,
  "名称" TEXT NOT NULL UNIQUE,
  "创建时间" TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);
INSERT INTO "用户新" SELECT * FROM "用户";
DROP TABLE "用户";
ALTER TABLE "用户新" RENAME TO "用户";
CREATE TABLE "邮箱账户" (
  "用户编号" INTEGER PRIMARY KEY REFERENCES "用户"("编号"),
  "邮箱" TEXT NOT NULL UNIQUE,
  "密码摘要" TEXT NOT NULL,
  "盐" TEXT NOT NULL,
  "迭代数" INTEGER NOT NULL,
  "条款版本" TEXT NOT NULL,
  "接受时间" INTEGER NOT NULL
);
CREATE TABLE "登录会话" (
  "摘要" TEXT PRIMARY KEY,
  "用户编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
  "到期" INTEGER NOT NULL
);
CREATE INDEX "登录会话_到期" ON "登录会话"("到期");
CREATE TABLE "账户限流" ("键" TEXT PRIMARY KEY, "次数" INTEGER NOT NULL, "到期" INTEGER NOT NULL);
-- 今释：旧服务可能留下尚未登记包名的待发布记录，保留这些发布者的所有权。
INSERT OR IGNORE INTO "包" ("名称", "所有者编号") SELECT "包名", "发布者编号" FROM "上传";
-- 古曰：非其主不得书。今释：事务级约束防止并发请求绕过所有权检查。
CREATE TRIGGER "上传所有者检查" BEFORE INSERT ON "上传"
WHEN NOT EXISTS (SELECT 1 FROM "包" WHERE "名称"=NEW."包名" AND "所有者编号"=NEW."发布者编号")
BEGIN SELECT RAISE(ABORT, '包不属于发布者'); END;
CREATE TRIGGER "版本所有者检查" BEFORE INSERT ON "包版本"
WHEN NOT EXISTS (SELECT 1 FROM "包" WHERE "名称"=NEW."包名" AND "所有者编号"=NEW."发布者编号")
BEGIN SELECT RAISE(ABORT, '包不属于发布者'); END;

PRAGMA defer_foreign_keys = OFF;
