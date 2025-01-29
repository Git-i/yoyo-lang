#include <math.h>
struct BigStruct {
    int a;
    int b;
    int c;
    int d;
};
struct SmallStruct {
    int a;
    int b;
};
__declspec(dllexport) struct BigStruct get_big_struct(float val) {
    return (struct BigStruct){10, 20, 55, 40};
}
__declspec(dllexport) struct SmallStruct get_small_struct(float val) {
    return (struct SmallStruct){100, 40};
}
