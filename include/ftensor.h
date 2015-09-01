
#ifdef FTENSOR_BOUNDS_CHECKS
#define BOUNDS_CHECK (check "bounds" __LINE__ __FILE__)
#else
#define BOUNDS_CHECK (noCheck)
#endif

#ifdef FTENSOR_UNSAFE_CHECKS
#define UNSAFE_CHECK (check "unsafe" __LINE__ __FILE__)
#else
#define UNSAFE_CHECK (noCheck)
#endif

#ifdef FTENSOR_INTERNAL_CHECKS
#define INTERNAL_CHECK (check "internal" __LINE__ __FILE__)
#else
#define INTERNAL_CHECK (noCheck)
#endif

