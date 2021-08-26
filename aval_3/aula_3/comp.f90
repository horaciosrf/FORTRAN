program testecomp
    implicit none
    real :: A,B,C
    complex :: raiz_1,raiz_2

    write(*,*)'A,B,C:'
    read(*,*) A,B,C

    call eq_quad(A,B,C,raiz_1,raiz_2)

    write(*,*)'raizes:'
    write(*,*)'R1:',real(raiz_1),'+ i ',aimag(raiz_1)
    write(*,*)'R2:',real(raiz_2),'+ i ',aimag(raiz_2)
    contains

    subroutine eq_quad(A,B,C,r1,r2)
        !Ax^2 + Bx + C = 0
        implicit none
        real, intent(in)  ::A,B,C
        complex,intent(out)  ::r1,r2
        complex :: delta, raiz_delta

        delta = cmplx(b**2 - 4*a*c,0.)
        raiz_delta = sqrt(delta)

        r1 = (-b + raiz_delta)/(2*a)
        r2 = (-b - raiz_delta)/(2*a)
    
    end subroutine eq_quad
end program testecomp