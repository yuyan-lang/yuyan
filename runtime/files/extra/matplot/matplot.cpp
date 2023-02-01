
// #include <matplot/matplot.h>
// #include "../../globalInclude.h"

// void yy_matlib_createLinePlot_cpp(
//     std::string filename,
//     std::string titleStr,
//     std::string xAsixName,
//     std::string yAsixName,
//     std::vector<double> xValuesList,
//     std::vector<std::vector<double>> ysValuesList,
//     std::vector<std::string> legendList
// ){

//     using namespace matplot;
//     // for (std::vector<std::vector<double>>::iterator p=ysValuesList.begin(); 
//     //     p!=ysValuesList.end(); ++p){
//     //     plot(xValuesList, *p);
//     // }
//     auto p = plot(xValuesList, xValuesList);
//     for (int i = 0; i < ysValuesList.size(); i++)
//     {
//         auto p2 = plot(xValuesList, ysValuesList[i]);
//         p2->display_name(legendList[i]);
//         if(i == 0){
//             hold(on);
//         }
//     }
//     hold(off);

//     title(titleStr);
//     xlabel(xAsixName);
//     ylabel(yAsixName);
//     // if (legendList.size() > 1) {
//     //     matplot::legend(legendList);
//     // }
//     matplot::legend();

//     // show();
//     save(filename);
// }

// extern "C" {

// void yy_matlib_createLinePlot(
//     char* filename,
//     char* titleStr,
//     char* xAsixName,
//     char* yAsixName,
//     double* xValuesList,
//     double** ysValuesList,
//     char** legendList,
//     int xValueLength,
//     int yValueLength
// ){

//     std::vector<std::vector<double>> ysValuesListVec;
//     std::vector<std::string> legendListVec;
//     for (int j = 0; j < yValueLength; j++)
//     {
//         ysValuesListVec.push_back(std::vector<double>(ysValuesList[j], ysValuesList[j] + xValueLength));
//         legendListVec.push_back(std::string(legendList[j]));
//     }
//     yy_matlib_createLinePlot_cpp(
//         std::string(filename),
//         std::string(titleStr),
//         std::string(xAsixName),
//         std::string(yAsixName),
//         std::vector<double>(xValuesList, xValuesList + xValueLength), 
//         ysValuesListVec,
//         legendListVec
//     );
// }

// }