module sistemas
    implicit none
    
    contains

    subroutine resolve_U(N,U,b,x)
    implicit none
    integer,intent(in) :: N
    real(8),intent(in) :: U(N,N), b(N)
    real(8),intent(out):: x(N)
    integer            :: i,j
    real(8)            :: soma

    x(N) = b(N)/U(N,N)

    do i = N-1, 1, -1

        soma=0.d0
        do j = i+1,N
            soma = soma + U(i,j)*x(j)
        end do
        x(i) = (b(i)-soma)/U(i,i)
    end do


    
    end subroutine resolve_U
    
end module sistemas

program teste
    use sistemas
    implicit none
    integer,parameter  ::N=100
    real(8)            :: A(N,N),b(N),x(N),x0(N)
    real(8)            :: ti,tf,dt,t
    integer            :: i,j


    call random_number(A)

    A=int(1000*(A-0.5d0))

    do j = 1, N
        A(j+1:N,j) = 0.d0
    end do

    ti=0.d0
    tf=10.d0
    dt = (tf-ti)/(N-1)
    do i = 1,N
        t=ti+(i-1)*dt
        x0(i) = sin(t)
    end do
    b = matmul(A,x0)

    call resolve_U(N,A,b,x)

    do i = 1, N
        write(*,*)t,x0(i),x(i)
    end do

    write(*,*) x

    
end program teste