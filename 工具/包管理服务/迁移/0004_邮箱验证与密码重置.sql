-- 古曰：验邮箱以通上传，验身份另有其籍。今释：邮箱验证与用户的身份认证标识完全独立。
ALTER TABLE "邮箱账户" ADD COLUMN "邮箱已验证" INTEGER NOT NULL DEFAULT 0 CHECK ("邮箱已验证" IN (0,1));
CREATE TABLE "邮件凭据" (
  "摘要" TEXT PRIMARY KEY,
  "用户编号" INTEGER NOT NULL REFERENCES "用户"("编号"),
  "用途" TEXT NOT NULL CHECK ("用途" IN ('verify','reset')),
  "到期" INTEGER NOT NULL
);
CREATE INDEX "邮件凭据_账户" ON "邮件凭据"("用户编号", "用途");
CREATE INDEX "邮件凭据_到期" ON "邮件凭据"("到期");
CREATE VIEW "上传授权" AS
SELECT t.* FROM "访问令牌" t JOIN "邮箱账户" a ON a."用户编号"=t."用户编号"
WHERE a."邮箱已验证"=1;
-- 古曰：旧符未验者废之。今释：旧令牌不能绕过新的邮箱要求；身份认证也不豁免邮箱验证。
UPDATE "访问令牌" SET "已启用"=0 WHERE "用户编号" NOT IN (SELECT "用户编号" FROM "邮箱账户" WHERE "邮箱已验证"=1);
