#include "../extra_include.h"

yy_ptr yyMatPlotCreateLinePlot(
    yy_ptr filenameAddr,
    yy_ptr titleAddr,
    yy_ptr xAsixNameAddr,
    yy_ptr yAsixNameAddr,
    yy_ptr xValuesListAddr,
    yy_ptr ysValuesListAddr,
    yy_ptr legendListAddr
){
    int xNum = iso_list_get_length(xValuesListAddr);
    int yNum = iso_list_get_length(ysValuesListAddr);
    double *xValuesList = GC_MALLOC(xNum * sizeof(double));
    double **ysValuesList = GC_MALLOC(yNum * sizeof(double));
    char **legendList = GC_MALLOC(yNum * sizeof(char *));

    yy_ptr* xValuesPtrs = iso_list_get_elements(xValuesListAddr);
    yy_ptr* ysValuesPtrs = iso_list_get_elements(ysValuesListAddr);
    yy_ptr* legendPtrs = iso_list_get_elements(legendList);

    for (int i = 0; i < xNum; i++){
        xValuesList[i] = addr_to_int(xValuesPtrs);
    }
    for (int j = 0; j < yNum; j ++){
        ysValuesList[j] = GC_MALLOC(xNum * sizeof(double));
        yy_ptr *yValuesPtrs = iso_list_get_elements(ysValuesListAddr);
        for (int i = 0; i < xNum; i++)
        {
            ysValuesList[j][i] = addr_to_int(yValuesPtrs[i]);
        }
    }

    for (int j = 0; j < yNum; j ++){
        legendList[j] = addr_to_string(legendPtrs[j]);
    }

    yy_matlib_createLinePlot(
        addr_to_string(filenameAddr),
        addr_to_string(titleAddr),
        addr_to_string(xAsixNameAddr),
        addr_to_string(yAsixNameAddr),
        xValuesList,
        ysValuesList,
        legendList,
        xNum,
        yNum );
}