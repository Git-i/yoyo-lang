#ifdef _MSC_VER
#ifdef YOYO_DLL
#define YOYO_API __declspec(dllexport)
#else
#define YOYO_API __declspec(dllimport)
#endif
#else
#define YOYO_API
#endif