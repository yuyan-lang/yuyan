#include "公共包含.h"
#include <zlib.h>

// 文言：归档不落客名之径，限量解之，验印乃用。
// 汉语：只读取经典 ZIP 的存储与 deflate 条目，不向文件系统解压；所有长度、类型和 CRC 先验证。
#define 包上限 (16u * 1024u * 1024u)
#define 展开上限 (64u * 1024u * 1024u)
#define 条目上限 2048
typedef struct { char *名; uint32_t 位, 压, 长, 印; uint16_t 法; } 档项;
typedef struct { unsigned char *字节; size_t 长; unsigned 数, 已见; char *路径[条目上限]; uint32_t 首[条目上限],尾[条目上限]; 档项 项[条目上限]; } 归档;
static unsigned 短数(const unsigned char *p) { return p[0] | (p[1]<<8); }
static uint32_t 长数(const unsigned char *p) { return 短数(p) | ((uint32_t)短数(p+2)<<16); }
static const char 六四[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static unsigned char *解码(const char *s, size_t *n) {
    size_t l=strlen(s); if (!l || l%4 || l>包上限*4/3+4) return NULL;
    unsigned char *b=malloc(l/4*3); if(!b)return NULL; size_t j=0;
    for(size_t i=0;i<l;i+=4) {
        unsigned v=0, pad=0;
        for(unsigned k=0;k<4;k++) {
            const char *p=strchr(六四,s[i+k]);
            if(s[i+k]=='=') { if(k<2 || i+4!=l){free(b);return NULL;} pad++; v<<=6; }
            else { if(!p||pad){free(b);return NULL;} v=(v<<6)|(unsigned)(p-六四); }
        }
        b[j++]=v>>16; if(pad<2)b[j++]=v>>8; if(!pad)b[j++]=v;
    }
    if(j>包上限){free(b);return NULL;} *n=j; return b;
}
static 豫言值 编码(const unsigned char *p,size_t n) {
    size_t l=(n+2)/3*4; char *s=malloc(l+1); if(!s)return 静态字符串转豫言值("!");
    size_t j=0; for(size_t i=0;i<n;i+=3) {
        unsigned v=(unsigned)p[i]<<16; if(i+1<n)v|=(unsigned)p[i+1]<<8; if(i+2<n)v|=p[i+2];
        s[j++]=六四[v>>18];s[j++]=六四[(v>>12)&63];s[j++]=i+1<n?六四[(v>>6)&63]:'=';s[j++]=i+2<n?六四[v&63]:'=';
    }
    s[l]=0; 豫言值 r=复制字节为豫言值(l,s); free(s);return r;
}
static int 路径有效(const unsigned char *s,size_t n) {
    if(!n||n>1024||s[0]=='/'||memchr(s,0,n))return 0;
    size_t 段=0;
    for(size_t i=0;i<n;) {
        unsigned c=s[i];
        if(c<32||c==127||c=='\\'||c==':')return 0;
        if(c=='/') {size_t l=i-段;if(!l||(l==1&&s[段]=='.')||(l==2&&s[段]=='.'&&s[段+1]=='.'))return 0;段=i+1;}
        if(c<128){i++;continue;}
        unsigned k=c>=0xf0?4:c>=0xe0?3:c>=0xc2?2:0, v=c&((1u<<(7-k))-1);
        if(!k||i+k>n)return 0;
        for(unsigned j=1;j<k;j++){if((s[i+j]&192)!=128)return 0;v=(v<<6)|(s[i+j]&63);}
        if((k==2&&v<128)||(k==3&&v<2048)||(k==4&&v<65536)||v>0x10ffff||(v>=0xd800&&v<=0xdfff))return 0;
        i+=k;
    }
    size_t l=n-段;return !((l==1&&s[段]=='.')||(l==2&&s[段]=='.'&&s[段+1]=='.'));
}
static unsigned char *解项(归档 *a,档项 *e) {
    unsigned char *b=malloc((size_t)e->长+1);if(!b)return NULL;
    int ok=0;
    if(e->法==0){ok=e->压==e->长;if(ok)memcpy(b,a->字节+e->位,e->长);}
    else {
        z_stream z={0};z.next_in=a->字节+e->位;z.avail_in=e->压;z.next_out=b;z.avail_out=e->长+1;
        if(inflateInit2(&z,-MAX_WBITS)==Z_OK){int r=inflate(&z,Z_FINISH);ok=r==Z_STREAM_END&&z.total_in==e->压&&z.total_out==e->长;inflateEnd(&z);}
    }
    if(!ok||(uint32_t)crc32(0,b,e->长)!=e->印){free(b);return NULL;}b[e->长]=0;return b;
}
static void 释放归档(归档 *a){if(a){for(unsigned i=0;i<a->数;i++)free(a->项[i].名);for(unsigned i=0;i<a->已见;i++)free(a->路径[i]);free(a->字节);free(a);}}
豫言值 豫言_归档打开(豫言值 文) {
    归档 *a=calloc(1,sizeof(*a));char *名单=NULL;size_t 名长=0;
    if(!a)goto 坏; a->字节=解码(豫言值转字符串(文),&a->长);if(!a->字节||a->长<22)goto 坏;
    size_t end=a->长-22;
    while(长数(a->字节+end)!=0x06054b50 || end+22+短数(a->字节+end+20)!=a->长){if(!end||a->长-end>65557)goto 坏;end--;}
    const unsigned char *d=a->字节+end;
    unsigned count=短数(d+10);size_t pos=长数(d+16), cd=pos;uint64_t total=0;
    if(短数(d+4)||短数(d+6)||短数(d+8)!=count||!count||count>条目上限||pos+长数(d+12)!=end)goto 坏;
    for(unsigned i=0;i<count;i++){
        if(pos+46>end||长数(a->字节+pos)!=0x02014b50)goto 坏;d=a->字节+pos;
        unsigned flags=短数(d+8),method=短数(d+10),nl=短数(d+28),extra=短数(d+30),comment=短数(d+32);
        uint32_t size=长数(d+24),packed=长数(d+20),off=长数(d+42),mode=长数(d+38)>>16;
        if(pos+46+nl+extra+comment>end||短数(d+34)||(flags&~0x080e)|| (method!=0&&method!=8)||size>包上限||packed>包上限||!路径有效(d+46,nl))goto 坏;
        unsigned type=mode&0170000;int dir=d[46+nl-1]=='/';
        if((type&&type!=0100000&&type!=0040000)||(type==0040000&&!dir)||(type==0100000&&dir)||(!dir&&(长数(d+38)&16)))goto 坏;
        for(unsigned j=0;j<a->已见;j++){
            size_t old=strlen(a->路径[j]),cur=nl-(dir?1:0);if(old&&a->路径[j][old-1]=='/')old--;
            if(old==cur&&!memcmp(a->路径[j],d+46,cur))goto 坏;
        }
        for(unsigned j=0;j<a->数;j++){
            size_t old=strlen(a->项[j].名),cur=nl-(dir?1:0);
            if(old==cur&&!memcmp(a->项[j].名,d+46,cur))goto 坏;
            if((old<cur&&!memcmp(a->项[j].名,d+46,old)&&d[46+old]=='/')||(cur<old&&!memcmp(a->项[j].名,d+46,cur)&&a->项[j].名[cur]=='/'&&!dir))goto 坏;
        }
        if((uint64_t)off+30>cd)goto 坏;const unsigned char *h=a->字节+off;
        unsigned hn=短数(h+26),he=短数(h+28);uint64_t start=(uint64_t)off+30+hn+he;
        if(长数(h)!=0x04034b50||短数(h+6)!=flags||短数(h+8)!=method||hn!=nl||start+packed>cd||memcmp(h+30,d+46,nl))goto 坏;
        for(unsigned j=0;j<a->已见;j++)if(off<a->尾[j]&&start+packed>a->首[j])goto 坏;
        a->路径[a->已见]=malloc(nl+1);if(!a->路径[a->已见])goto 坏;
        memcpy(a->路径[a->已见],d+46,nl);a->路径[a->已见][nl]=0;
        a->首[a->已见]=off;a->尾[a->已见]=start+packed;a->已见++;
        if(!(flags&8)&&(长数(h+14)!=长数(d+16)||长数(h+18)!=packed||长数(h+22)!=size))goto 坏;
        total+=size;if(total>展开上限|| (dir&&size))goto 坏;
        档项 e={NULL,(uint32_t)start,packed,size,长数(d+16),(uint16_t)method};
        unsigned char *body=解项(a,&e);if(!body)goto 坏;free(body);
        if(!dir){
            e.名=malloc(nl+1);if(!e.名)goto 坏;memcpy(e.名,d+46,nl);e.名[nl]=0;
            a->项[a->数++]=e;char *next=realloc(名单,名长+nl+2);if(!next)goto 坏;名单=next;
            memcpy(名单+名长,e.名,nl);名长+=nl;名单[名长++]='\n';名单[名长]=0;
        }
        pos+=46+nl+extra+comment;
    }
    if(pos!=end||!a->数)goto 坏;
    豫言值 r=元组转豫言值(2,(豫言值[]){整数转豫言值((intptr_t)a),复制字节为豫言值(名长,名单)});free(名单);return r;
坏:
    free(名单);释放归档(a);
    return 元组转豫言值(2,(豫言值[]){整数转豫言值(0),静态字符串转豫言值("ZIP 无效、路径冲突、特殊文件或超过限制")});
}
豫言值 豫言_归档读取(豫言值 柄,豫言值 序,豫言值 文本) {
    归档 *a=(归档 *)(intptr_t)豫言值转整数(柄);int64_t i=豫言值转整数(序);
    if(!a||i<0||(uint64_t)i>=a->数)return 静态字符串转豫言值("");
    档项 *e=&a->项[i];unsigned char *b=解项(a,e);if(!b)return 静态字符串转豫言值("!");
    if(豫言值转整数(文本)&&memchr(b,0,e->长)){free(b);return 静态字符串转豫言值("");}
    豫言值 r=豫言值转整数(文本)?复制字节为豫言值(e->长,b):编码(b,e->长);free(b);return r;
}
豫言值 豫言_归档关闭(豫言值 柄){释放归档((归档 *)(intptr_t)豫言值转整数(柄));return 整数转豫言值(0);}
// 文言：名用常字，不纳隐符。汉语：固定字符集避免空白、控制字符及大小写折叠歧义。
豫言值 豫言_所有者名称有效(豫言值 文) {
    const unsigned char *s=(const unsigned char *)豫言值转字符串(文);size_t n=strlen((const char *)s),count=0;
    if(!路径有效(s,n)||!strncmp((const char *)s,"用户-",strlen("用户-")))return 整数转豫言值(0);
    for(size_t i=0;i<n;count++){
        unsigned c=s[i],k=c<128?1:c>=0xf0?4:c>=0xe0?3:2,v=k==1?c:c&((1u<<(7-k))-1);
        for(unsigned j=1;j<k;j++)v=(v<<6)|(s[i+j]&63);
        if(!((v>='a'&&v<='z')||(v>='0'&&v<='9')||v=='_'||v=='-'||(v>=0x3400&&v<=0x9fff)||(v>=0x20000&&v<=0x323af)))return 整数转豫言值(0);
        i+=k;
    }
    return 整数转豫言值(count>0&&count<=32);
}
