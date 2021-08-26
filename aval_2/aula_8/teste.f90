module integrais

    implicit none
    integer,parameter::pr = kind(1.d0)
    real(pr),parameter  ::pi = 3.14159265359d0
    contains
    subroutine trapezios(FUN,a,b,S )
        implicit none
        real(pr),external       :: FUN
        real(pr),intent(in)     :: a,b
        real(pr),intent(out)    ::  S
        real(pr)                :: soma,sex,h,x
        real(pr)                :: S1
        real(pr)                :: eps
        integer             :: i,N
        
        eps = 1.d-6
        N = 4
        h = (b-a)/N
        sex = FUN(a) + FUN(b)
        soma = 0.
        x = a+h
        do i = 1, N-1
            soma = soma + FUN(x)
            x = x + h
        end do
        S1 = h * (sex + 2*soma)/2.d0
        
        do 
            x = a - h/2.d0
            do i = 1, N
                x = x + h
                soma = soma + FUN(x)
            end do
            h = h / 2.d0
            S = h * (sex + 2*soma )/2.d0
            if (abs(S-S1) > eps) then
                N = 2 * N
                S1 = S
            else
                exit
            end if
        end do
    
    
    end subroutine trapezios

     real(pr) function xquad(x) 
            implicit none
            real(pr),intent(in)  ::x
            xquad = x**2
    end function xquad

    real(pr) function seno(x) 
            implicit none
            real(pr),intent(in)  ::x
            seno = sin(x)
    end function seno

    real(pr) function x_1(x) 
            implicit none
            real(pr),intent(in)  ::x
            x_1= 1.d0/x
    end function x_1

end module integrais



program integral_trap
    use integrais
    implicit none

    real(pr) :: a!,b
    !integer ::N
    real(pr)::area,x

    ! a = 0.
    ! b = 1.
     !N = 100000

    ! call trapezios(xquad,a,b,N,area )


    ! write(*,*)area

    ! a = 0.
    ! b = pi

    ! call trapezios(seno,a,b,N,area )

    
    ! write(*,*)area

    a = 1.
    read(*,*)x

    call trapezios(x_1,a,x,area )

    write(*,*)log(x)
    write(*,*)area


    
end program integral_trap