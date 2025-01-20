#define Z_PREFIX_SET
#include "../zlib/zlib.h"

ZEXTERN int ZEXPORT inflateInit(z_streamp strm) { return z_inflateInit(strm); }
