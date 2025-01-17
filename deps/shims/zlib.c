#define Z_PREFIX_SET
#include "../zlib-1.3.1/zlib.h"

ZEXTERN int ZEXPORT inflateInit(z_streamp strm) { return z_inflateInit(strm); }
