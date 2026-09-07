-- 文言：一户一名，既定不易；旧官名仍存。
-- 汉语：名称绑定为一次性操作，保留已有正式名称，随机账户名尚未绑定。
ALTER TABLE "用户" ADD COLUMN "所有者已绑定" INTEGER NOT NULL DEFAULT 0 CHECK ("所有者已绑定" IN (0,1));
UPDATE "用户" SET "所有者已绑定"=1 WHERE "名称" NOT LIKE '用户-%';
CREATE TRIGGER "所有者名称不变" BEFORE UPDATE OF "名称","所有者已绑定" ON "用户"
WHEN OLD."所有者已绑定"=1 AND (NEW."名称"<>OLD."名称" OR NEW."所有者已绑定"<>1)
BEGIN SELECT RAISE(ABORT,'所有者名称不可修改'); END;
ALTER TABLE "即时版本" ADD COLUMN "归档摘要" TEXT;
DROP VIEW "上传授权";
CREATE VIEW "上传授权" AS
SELECT t.* FROM "访问令牌" t JOIN "邮箱账户" a ON a."用户编号"=t."用户编号"
JOIN "用户" u ON u."编号"=t."用户编号"
WHERE a."邮箱已验证"=1 AND u."所有者已绑定"=1;
