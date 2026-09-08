/* 文言：同题直书于 C，专其型；汉语：手写 C 的专用类型参照，非豫言后端等价物。 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
static uint64_t 斐波那契(uint64_t 数) { return 数 < 2 ? 数 : 斐波那契(数-1) + 斐波那契(数-2); }
typedef struct 树 { struct 树 *左, *右; uint64_t 值; } 树;
static 树 *建树(uint64_t 深) {
 树 *物 = malloc(sizeof *物); if (!物) abort();
 物->值 = 1; 物->左 = 深 ? 建树(深-1) : NULL; 物->右 = 深 ? 建树(深-1) : NULL; return 物;
}
static uint64_t 树和(树 *物) { return 物->左 ? 树和(物->左)+树和(物->右) : 物->值; }
static void 释树(树 *物) { if (物->左) { 释树(物->左); 释树(物->右); } free(物); }
int main(int 数, char **参数) {
 if (数 != 3) return 2; uint64_t 入 = strtoull(参数[2], NULL, 10), 果;
 if (!strcmp(参数[1], "斐波那契")) 果 = 斐波那契(入);
 else if (!strcmp(参数[1], "树基准")) { 树 *根 = 建树(入); 果 = 树和(根); 释树(根); }
 else if (!strcmp(参数[1], "数组基准")) {
  uint64_t *组 = malloc(入*sizeof *组); if (!组 && 入) return 1;
  for (uint64_t 位=入; 位; --位) 组[位-1]=位;
  果=0; for(uint64_t 位=入; 位; --位) 果+=组[位-1]; free(组);
 } else return 2;
 printf("%" PRIu64 "\n", 果); return 0;
}
