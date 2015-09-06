#ifdef FTENSOR_BOUNDS_CHECKS
#define BOUNDS_CHECK (Math.FTensor.Internal.Check.check "bounds" __LINE__ __FILE__)
#else
#define BOUNDS_CHECK (Math.FTensor.Internal.Check.noCheck)
#endif

#ifdef FTENSOR_UNSAFE_CHECKS
#define UNSAFE_CHECK (Math.FTensor.Internal.Check.check "unsafe" __LINE__ __FILE__)
#else
#define UNSAFE_CHECK (Math.FTensor.Internal.Check.noCheck)
#endif

#ifdef FTENSOR_INTERNAL_CHECKS
#define INTERNAL_CHECK (Math.FTensor.Internal.Check.check "internal" __LINE__ __FILE__)
#else
#define INTERNAL_CHECK (Math.FTensor.Internal.Check.noCheck)
#endif
