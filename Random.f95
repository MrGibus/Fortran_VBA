subroutine RndInt(iRnd)
    !generate random number based on system time first seed
    implicit none
    !GCC$ ATTRIBUTES DLLEXPORT,STDCALL:: RNDINT

    integer,dimension(8)    ::  Time  !this inserts the initial seed for the random number
    integer,dimension(12)   ::  put
    real*8                  ::  rnd
    integer *2              ::  iRnd , iMax, iMin, irange

    iMin=0
    iMax=10
    call    date_and_time(VALUES=Time)
    put=Time(8)*1000
    call random_seed(PUT=put)
    Call random_number(rnd)
    irange = abs(iMin-iMax)
    iRnd = nint(rnd*(irange)+iMin)

end subroutine


function FRndInt(iMin, iMax)
    !generate random number based on system time first seed
    implicit none
    !GCC$ ATTRIBUTES DLLEXPORT,STDCALL:: FRNDINT

    integer,dimension(8)    ::  Time  !this inserts the initial seed for the random number
    integer,dimension(12)   ::  put
    real*8                  ::  rnd
    integer*2               ::  iMin, iMax
    integer*2               ::  iRnd, iRange
    integer*2               ::  FRndInt

    call    date_and_time(VALUES=Time)
    put=Time(8)*1000
    call random_seed(PUT=put)
    Call random_number(rnd)
    iRange = abs(iMin-iMax)
    iRnd = nint(rnd*(iRange)+iMin)
    FRndInt=iRnd
end function


function MYFUNCTION(a,b)
    !GCC$ ATTRIBUTES DLLEXPORT,STDCALL:: MYFUNCTION

    integer*2 ::  a,b
    integer*2  ::  MYFUNCTION

    MYFUNCTION = a-b

end function
