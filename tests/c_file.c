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
__declspec(dllexport) struct BigStruct get_big_struct(int val) {
    return (struct BigStruct){val, val * 30, 55, 40};
}
__declspec(dllexport) struct SmallStruct get_small_struct(int val) {
    return (struct SmallStruct){val, val * 40};
}

__declspec(dllexport) int get_int(int val) {
    return val * 5;
}

