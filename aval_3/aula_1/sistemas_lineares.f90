module sistemas1
    implicit none

    integer, parameter:: prc = kind(0.d0) 


    CONTAINS

    subroutine Retrosubst(N, U, b, x)
        ! Esta subrotina considera que todos os elementos ds
        ! triangular inferior sao iguais a zero
        implicit none
        integer, intent(in):: N
        Real(prc), intent(in):: U(N,N), b(N)
        Real(prc), intent(out):: x(N)
        integer:: i, j
        real(prc):: soma

        x(N) = b(N) / U(N,N)

        do i = N-1, 1, -1
            Soma = 0.d0
            do j = i+1, N
                soma = soma + U(i,j)*x(j)
            end do
            x(i) = (b(i) - soma) / u(i,i)
        end do

    end subroutine Retrosubst

    subroutine Elimina(N, A, b)
    ! Esta versao nao verifica a qualidade da matriz, pressupondo que é não-singular
        implicit none
        integer, intent(in):: N
        real(prc), intent(inout):: A(N,N), b(N)
        real(prc):: m
        integer:: i, j, k

        do k = 1, N-1
            do i = k+1, N
                m = A(i,k)/A(k,k)
        !        A(i,:) = A(i,:) - m * A(k,:)
                do j = k+1, N
                    A(i,j) = A(i,j) - m*A(k,j)
                end do
                b(i) = b(i) - m*b(k)
            end do
        end do

    end subroutine Elimina

    subroutine Elimina_pivot(N, A, b, erro)
        ! Esta versao nao verifica a qualidade da matriz, pressupondo que é não-singular
        implicit none
        integer, intent(in):: N
        real(prc), intent(inout):: A(N,N), b(N)
        integer, intent(out):: erro
        real(prc):: m
        real(prc):: eps = 1.d-8
        real(prc):: aux, pivo
        integer:: i, j, k, Lp 

        do k = 1, N-1
        !!!!! Pivoteamento
            erro = 0
            pivo = a(k,k)
            Lp = k
            do i = k+1, N
                if ( abs(a(i,k)) > abs(pivo) ) then
                    pivo = a(i,k)
                    Lp = i
                end if
            end do
            if ( abs(pivo) < eps ) then
                if ( Lp /= k ) then
                    do j = k, N
                        aux = a(k,j)
                        a(k,k) = a(Lp,j)
                        a(Lp,j) = aux
                    end do
                    aux = b(k)
                    b(k) = b(Lp)
                    b(Lp) = aux
                end if
                !write(*,*) A(k,k)
        !!!!! Eliminação
                do i = k+1, N
                    m = A(i,k) / A(k,k)
                    do j = k+1, N
                        A(i,j) = A(i,j) - m*A(k,j)
                    end do
                    b(i) = b(i) - m*b(k)
                end do
            else
                erro = 1
                !Erro igual a 1 significa que nao ha solucao
                exit
            end if
        end do
        !write(*,*) A(N,N)
        if (abs(A(N,N)) < eps) erro = 1

    end subroutine Elimina_pivot

    subroutine FATORA_LU(N, A)
        ! Esta versao nao verifica a qualidade da matriz, pressupondo que é não-singular
        implicit none
        integer, intent(in):: N
        real(prc), intent(inout):: A(N,N)
        real(prc):: m
        integer:: i, j, k

        do k = 1, N-1
            do i = k+1, N
                m = A(i,k)/A(k,k)
                do j = k+1, N
                    A(i,j) = A(i,j) - m*A(k,j)
                end do
                A(i,k) = m
            end do
        end do

        end subroutine FATORA_LU

        subroutine subst_direta(N, L, b, y)
            implicit none
            integer,intent(in)    :: N
            real(prc),intent(in)  ::  L(N,N),b(N)
            real(prc),intent(out) :: y(N)
            integer               :: i,j
            real(prc)             :: soma
        
            y(1) = b(1)
            do i = 2, N
                soma = 0.d0
                do j = 1, i-1 
                    soma = soma + L(i,j) * y(j)
                end do
                y(i) = b(i) - soma
            end do
        end subroutine subst_direta

end module sistemas1

program teste_sistema
    use sistemas1
    implicit none
    integer, parameter:: N = 100
    real(prc):: A(N,N), b(N), x(N), x0(N), t(N),y(N)
    real(prc):: ti, tf, dt
    integer:: i!, erro
    call random_number( A )

    ti = 0.d0
    tf = 10.d0
    dt = (tf-ti)/(N-1)

    do i = 1, N
        t(i) = ti + (i-1)*dt
        x0(i) = sin(t(i))
    end do
    b = matmul(A,x0)

    !A(:,500) = A(:,1)

    ! A(1,:) = [2.d0, 1.d0, -1.d0]
    ! A(2,:) = [-1.d0, 4.d0, 1.d0]
    ! A(3,:) = [1.d0, 5.d0, 0.d0]
    ! b = [3.d0, 5.d0, 8.d0]

    call FATORA_LU(N,A)
    call subst_direta(N,A,b,y)
    call Retrosubst(N,A,y,x)

    ! call Elimina_pivot(N, A, b, erro)
    !call Elimina(N, A, b)
    ! write(*,*) 'Erro: ', erro
    ! call Retrosubst(N,A,b,x)

    open(33, file ='solucao.dat',status='replace',action='write')
    do i = 1, N
       write(*,*) t(i), x0(i), x(i)
       write(33,*) t(i), x0(i), x(i)
    end do
end program