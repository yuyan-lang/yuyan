#include "公共包含.h"
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>

/* 文言：密钥皆定三十二字节；汉语：X25519 与 Ed25519 的原始私钥、公钥长度均为 32 字节。 */
static void 校验三十二字节(豫言值 内容, const char *错误) {
    if (获取豫言_字节串长度(内容) != 32) 报错并中止((char *)错误);
}

豫言值 豫言_密码_SHA256(豫言值 内容) {
    unsigned char 摘要[32];
    unsigned int 长度 = 0;
    if (EVP_Digest(豫言值转字节串指针(内容),
                   (size_t)获取豫言_字节串长度(内容), 摘要, &长度,
                   EVP_sha256(), NULL) != 1 || 长度 != sizeof(摘要)) {
        报错并中止("SHA-256 摘要失败");
    }
    return 复制字节为豫言值(sizeof(摘要), 摘要);
}

/* 文言：隐名簿循 OpenSSH 旧式 HMAC-SHA1，盐定二十字。汉语：仅供 known_hosts 哈希主机名匹配，20 字节盐作 HMAC-SHA1 密钥。 */
豫言值 豫言_密码_已知主机HMACSHA1(豫言值 盐, 豫言值 主机) {
    if (获取豫言_字节串长度(盐) != 20 || 获取豫言_字节串长度(主机) > 256) {
        报错并中止("已知主机 HMAC-SHA1 参数长度非法");
    }
    unsigned char 摘要[EVP_MAX_MD_SIZE];
    unsigned int 长度 = 0;
    if (HMAC(EVP_sha1(), 豫言值转字节串指针(盐), 20,
             豫言值转字节串指针(主机),
             (size_t)获取豫言_字节串长度(主机), 摘要, &长度) == NULL ||
        长度 != 20) {
        报错并中止("已知主机 HMAC-SHA1 计算失败");
    }
    豫言值 结果 = 复制字节为豫言值(长度, 摘要);
    OPENSSL_cleanse(摘要, sizeof(摘要));
    return 结果;
}

static 豫言值 求原始公钥(豫言值 私种, int 算法, const char *错误) {
    校验三十二字节(私种, 错误);
    EVP_PKEY *私钥 = EVP_PKEY_new_raw_private_key(
        算法, NULL, 豫言值转字节串指针(私种), 32);
    if (私钥 == NULL) 报错并中止((char *)错误);
    unsigned char 公钥[32];
    size_t 长度 = sizeof(公钥);
    int 成功 = EVP_PKEY_get_raw_public_key(私钥, 公钥, &长度);
    EVP_PKEY_free(私钥);
    if (成功 != 1 || 长度 != sizeof(公钥)) 报错并中止((char *)错误);
    return 复制字节为豫言值(sizeof(公钥), 公钥);
}

豫言值 豫言_密码_X25519公钥(豫言值 私种) {
    return 求原始公钥(私种, EVP_PKEY_X25519, "X25519 公钥生成失败");
}

豫言值 豫言_密码_X25519共密(豫言值 私种, 豫言值 对方公钥) {
    校验三十二字节(私种, "X25519 私钥长度必须为三十二字节");
    校验三十二字节(对方公钥, "X25519 公钥长度必须为三十二字节");
    EVP_PKEY *私钥 = EVP_PKEY_new_raw_private_key(
        EVP_PKEY_X25519, NULL, 豫言值转字节串指针(私种), 32);
    EVP_PKEY *公钥 = EVP_PKEY_new_raw_public_key(
        EVP_PKEY_X25519, NULL, 豫言值转字节串指针(对方公钥), 32);
    EVP_PKEY_CTX *上下文 = 私钥 == NULL ? NULL : EVP_PKEY_CTX_new(私钥, NULL);
    unsigned char 共密[32];
    size_t 长度 = sizeof(共密);
    int 成功 = 上下文 != NULL && 公钥 != NULL &&
        EVP_PKEY_derive_init(上下文) == 1 &&
        EVP_PKEY_derive_set_peer(上下文, 公钥) == 1 &&
        EVP_PKEY_derive(上下文, 共密, &长度) == 1 && 长度 == sizeof(共密);
    EVP_PKEY_CTX_free(上下文);
    EVP_PKEY_free(公钥);
    EVP_PKEY_free(私钥);
    if (!成功) 报错并中止("X25519 共密计算失败");
    /* 文言：合诸字验非全零，不据首异而早返；汉语：按 RFC 7748 对所有字节作或运算。 */
    unsigned char 非零 = 0;
    for (size_t 序 = 0; 序 < sizeof(共密); ++序) 非零 |= 共密[序];
    if (非零 == 0) {
        OPENSSL_cleanse(共密, sizeof(共密));
        报错并中止("X25519 共密不得全零");
    }
    豫言值 返回值 = 复制字节为豫言值(sizeof(共密), 共密);
    OPENSSL_cleanse(共密, sizeof(共密));
    return 返回值;
}

豫言值 豫言_密码_Ed25519公钥(豫言值 私种) {
    return 求原始公钥(私种, EVP_PKEY_ED25519, "Ed25519 公钥生成失败");
}

豫言值 豫言_密码_Ed25519签(豫言值 私种, 豫言值 正文) {
    校验三十二字节(私种, "Ed25519 私钥长度必须为三十二字节");
    EVP_PKEY *私钥 = EVP_PKEY_new_raw_private_key(
        EVP_PKEY_ED25519, NULL, 豫言值转字节串指针(私种), 32);
    EVP_MD_CTX *上下文 = EVP_MD_CTX_new();
    unsigned char 签名[64];
    size_t 长度 = sizeof(签名);
    int 成功 = 私钥 != NULL && 上下文 != NULL &&
        EVP_DigestSignInit(上下文, NULL, NULL, NULL, 私钥) == 1 &&
        EVP_DigestSign(上下文, 签名, &长度,
                       豫言值转字节串指针(正文),
                       (size_t)获取豫言_字节串长度(正文)) == 1 &&
        长度 == sizeof(签名);
    EVP_MD_CTX_free(上下文);
    EVP_PKEY_free(私钥);
    if (!成功) 报错并中止("Ed25519 签名失败");
    return 复制字节为豫言值(sizeof(签名), 签名);
}

豫言值 豫言_密码_Ed25519验(豫言值 公钥, 豫言值 正文, 豫言值 签名) {
    校验三十二字节(公钥, "Ed25519 公钥长度必须为三十二字节");
    if (获取豫言_字节串长度(签名) != 64) {
        报错并中止("Ed25519 签名长度必须为六十四字节");
    }
    EVP_PKEY *钥 = EVP_PKEY_new_raw_public_key(
        EVP_PKEY_ED25519, NULL, 豫言值转字节串指针(公钥), 32);
    EVP_MD_CTX *上下文 = EVP_MD_CTX_new();
    if (钥 == NULL || 上下文 == NULL ||
        EVP_DigestVerifyInit(上下文, NULL, NULL, NULL, 钥) != 1) {
        EVP_MD_CTX_free(上下文);
        EVP_PKEY_free(钥);
        报错并中止("Ed25519 验签初始化失败");
    }
    int 结果 = EVP_DigestVerify(上下文, 豫言值转字节串指针(签名), 64,
        豫言值转字节串指针(正文), (size_t)获取豫言_字节串长度(正文));
    EVP_MD_CTX_free(上下文);
    EVP_PKEY_free(钥);
    if (结果 < 0) 报错并中止("Ed25519 验签执行失败");
    return 爻转豫言值(结果 == 1);
}

/* 文言：限单报之量，防整数溢与外来巨报；汉语：SSH 报文原语每次最多处理 1 MiB。 */
#define 密码单报上限 (1024 * 1024)

static void 校验GCM参数(豫言值 钥, 豫言值 随机数, 豫言值 附加文, uint64_t 正文长度) {
    if (获取豫言_字节串长度(钥) != 32 || 获取豫言_字节串长度(随机数) != 12) {
        报错并中止("AES-256-GCM 密钥须为三十二字节，IV 须为十二字节");
    }
    if (获取豫言_字节串长度(附加文) > 密码单报上限 || 正文长度 > 密码单报上限) {
        报错并中止("AES-256-GCM 单报长度超过一兆字节");
    }
}

豫言值 豫言_密码_AES256GCM加密(豫言值 钥, 豫言值 随机数,
                                  豫言值 附加文, 豫言值 明文) {
    uint64_t 明文长 = 获取豫言_字节串长度(明文);
    校验GCM参数(钥, 随机数, 附加文, 明文长);
    EVP_CIPHER_CTX *上下文 = EVP_CIPHER_CTX_new();
    unsigned char *密文 = malloc((size_t)明文长 + 16);
    if (上下文 == NULL || 密文 == NULL) 报错并中止("AES-256-GCM 加密分配失败");
    int 附加长 = 0;
    int 密文长 = 0;
    int 末长 = 0;
    int 成功 = EVP_EncryptInit_ex(上下文, EVP_aes_256_gcm(), NULL, NULL, NULL) == 1 &&
        EVP_CIPHER_CTX_ctrl(上下文, EVP_CTRL_GCM_SET_IVLEN, 12, NULL) == 1 &&
        EVP_EncryptInit_ex(上下文, NULL, NULL,
            豫言值转字节串指针(钥), 豫言值转字节串指针(随机数)) == 1;
    if (成功 && 获取豫言_字节串长度(附加文) != 0) {
        成功 = EVP_EncryptUpdate(上下文, NULL, &附加长,
            豫言值转字节串指针(附加文),
            (int)获取豫言_字节串长度(附加文)) == 1;
    }
    if (成功) {
        成功 = EVP_EncryptUpdate(上下文, 密文, &密文长,
            豫言值转字节串指针(明文), (int)明文长) == 1 &&
            EVP_EncryptFinal_ex(上下文, 密文 + 密文长, &末长) == 1 &&
            (uint64_t)(密文长 + 末长) == 明文长 &&
            EVP_CIPHER_CTX_ctrl(上下文, EVP_CTRL_GCM_GET_TAG, 16,
                密文 + 明文长) == 1;
    }
    EVP_CIPHER_CTX_free(上下文);
    if (!成功) {
        free(密文);
        报错并中止("AES-256-GCM 加密失败");
    }
    豫言值 返回值 = 复制字节为豫言值(明文长 + 16, 密文);
    free(密文);
    return 返回值;
}

/* 文言：首字节一为验成，零为验败；汉语：私有 FFI 结果首字节表示认证状态。 */
豫言值 豫言_密码_AES256GCM解密结果(豫言值 钥, 豫言值 随机数,
                                      豫言值 附加文, 豫言值 密文与签) {
    uint64_t 总长 = 获取豫言_字节串长度(密文与签);
    校验GCM参数(钥, 随机数, 附加文, 总长 < 16 ? 0 : 总长 - 16);
    if (总长 < 16) {
        unsigned char 失败 = 0;
        return 复制字节为豫言值(1, &失败);
    }
    uint64_t 密文长 = 总长 - 16;
    EVP_CIPHER_CTX *上下文 = EVP_CIPHER_CTX_new();
    unsigned char *明文 = malloc((size_t)密文长 + 1);
    if (上下文 == NULL || 明文 == NULL) 报错并中止("AES-256-GCM 解密分配失败");
    int 附加长 = 0;
    int 已解长 = 0;
    int 末长 = 0;
    int 成功 = EVP_DecryptInit_ex(上下文, EVP_aes_256_gcm(), NULL, NULL, NULL) == 1 &&
        EVP_CIPHER_CTX_ctrl(上下文, EVP_CTRL_GCM_SET_IVLEN, 12, NULL) == 1 &&
        EVP_DecryptInit_ex(上下文, NULL, NULL,
            豫言值转字节串指针(钥), 豫言值转字节串指针(随机数)) == 1;
    if (成功 && 获取豫言_字节串长度(附加文) != 0) {
        成功 = EVP_DecryptUpdate(上下文, NULL, &附加长,
            豫言值转字节串指针(附加文),
            (int)获取豫言_字节串长度(附加文)) == 1;
    }
    if (成功) {
        成功 = EVP_DecryptUpdate(上下文, 明文 + 1, &已解长,
            豫言值转字节串指针(密文与签), (int)密文长) == 1 &&
            EVP_CIPHER_CTX_ctrl(上下文, EVP_CTRL_GCM_SET_TAG, 16,
                豫言值转字节串指针(密文与签) + 密文长) == 1;
    }
    if (!成功) {
        EVP_CIPHER_CTX_free(上下文);
        OPENSSL_cleanse(明文, (size_t)密文长 + 1);
        free(明文);
        报错并中止("AES-256-GCM 解密初始化失败");
    }
    int 已认证 = EVP_DecryptFinal_ex(上下文, 明文 + 1 + 已解长, &末长) == 1 &&
        (uint64_t)(已解长 + 末长) == 密文长;
    EVP_CIPHER_CTX_free(上下文);
    if (!已认证) {
        OPENSSL_cleanse(明文, (size_t)密文长 + 1);
        free(明文);
        unsigned char 失败 = 0;
        return 复制字节为豫言值(1, &失败);
    }
    明文[0] = 1;
    豫言值 返回值 = 复制字节为豫言值(密文长 + 1, 明文);
    OPENSSL_cleanse(明文, (size_t)密文长 + 1);
    free(明文);
    return 返回值;
}
