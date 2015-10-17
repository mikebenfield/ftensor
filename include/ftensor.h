#ifdef FTENSOR_BOUNDS_CHECKS
#define BOUNDS_CHECK (Math.Ftensor.Internal.Check.check "bounds" __LINE__ __FILE__)
#else
#define BOUNDS_CHECK (Math.Ftensor.Internal.Check.noCheck)
#endif

#ifdef FTENSOR_UNSAFE_CHECKS
#define UNSAFE_CHECK (Math.Ftensor.Internal.Check.check "unsafe" __LINE__ __FILE__)
#else
#define UNSAFE_CHECK (Math.Ftensor.Internal.Check.noCheck)
#endif

#ifdef FTENSOR_INTERNAL_CHECKS
#define INTERNAL_CHECK (Math.Ftensor.Internal.Check.check "internal" __LINE__ __FILE__)
#else
#define INTERNAL_CHECK (Math.Ftensor.Internal.Check.noCheck)
#endif
