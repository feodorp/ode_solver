module kinds
  integer, parameter :: real32 = SELECTED_REAL_KIND( 6, 37 )     ! single precision (4 bytes)
  integer, parameter :: real64 = SELECTED_REAL_KIND( 15, 307 )   ! double precision (8 bytes)
  integer, parameter :: real80 = SELECTED_REAL_KIND( 18, 4931 ) ! extended precision (10 bytes)
  integer, parameter :: int16 = SELECTED_INT_KIND( 4 )  ! short integer (2 bytes)
  integer, parameter :: int32 = SELECTED_INT_KIND( 9 )  ! normal integer (4 bytes)
  integer, parameter :: int64 = SELECTED_INT_KIND( 18 ) ! long integer (8 bytes)

! maximum values of integers
  integer(kind=int16), parameter :: int16_max = huge(0_int16)
  integer(kind=int32), parameter :: int32_max = huge(0_int32)
  integer(kind=int64), parameter :: int64_max = huge(0_int64)

! default precision
  integer, parameter :: rp = real64
  integer, parameter :: ip = int32
end module kinds

