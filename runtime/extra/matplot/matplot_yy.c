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
    double *xValuesList = yy_gcAllocateBytes(xNum * sizeof(double));
    double **ysValuesList = yy_gcAllocateBytes(yNum * sizeof(double));
    char **legendList = yy_gcAllocateBytes(yNum * sizeof(char *));

    yy_ptr* xValuesPtrs = iso_list_get_elements(xValuesListAddr);
    yy_ptr* ysValuesPtrs = iso_list_get_elements(ysValuesListAddr);
    yy_ptr* legendPtrs = iso_list_get_elements(legendListAddr);

    for (int i = 0; i < xNum; i++){
        xValuesList[i] = addr_to_double(xValuesPtrs[i]);
    }
    for (int j = 0; j < yNum; j ++){
        ysValuesList[j] = yy_gcAllocateBytes(xNum * sizeof(double));
        yy_ptr *yValuesPtrs = iso_list_get_elements(ysValuesPtrs[j]);
        for (int i = 0; i < xNum; i++)
        {
            ysValuesList[j][i] = addr_to_double(yValuesPtrs[i]);
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
    return unit_to_addr();
}