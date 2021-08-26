module estatistica
    implicit none
    
contains
    subroutine calcula_media_dp(N, x, m, dp)
        implicit none
        integer            :: i
        integer,intent(in) :: N
        real,intent(in) ::  x(N)
        real, intent(out) :: m, dp
        real     :: soma, soma_2


        do i = 1, N
            soma = soma + x(i)
            soma_2 = soma_2 + x(i)**2
        end do

        m = soma/N
        dp = sqrt(soma_2/N -(soma/N)**2)

    end subroutine calcula_media_dp

    real function media(N,x)
        implicit none
        integer,intent(in) :: N
        real,intent(in)    :: x(N)
        real               :: soma
        integer            :: i

        do i = 1, N
            soma = soma + x(i)
        end do

        media = soma/N
    end function media

    subroutine mediana(N,v,mn)
        implicit none
        integer, intent(in) :: N
        real,intent(in) :: v(N)
        real            :: x(N)
        integer         :: i, j
        real,intent(out) ::  mn
        mn = 0.
        x = v
        call ordenar(x,N)

        if ( MOD(N,2) == 0 ) then
            i = N/2
            j = i + 1
            mn = (x(i)+x(j))/2.
        else
            i = (N+2)/2
            mn = x(i)
            
        end if
    
    end subroutine mediana

    subroutine ordenar(v,N)
        implicit none
        integer,intent(in) :: N
        real,intent(inout) ::  v(N)
        real               :: aux
        integer            :: i,j, troca

        
        i = 0
        do 
            i = i + 1
            troca = 0
            do j = 1, N-i

                if ( v(j) > v(j+1)) then
                    troca = 1
                    aux = v(j)
                    v(j) = v(j+1)
                    v(j+1) = aux

                end if
                
            end do
            if ( troca == 0 ) exit
        end do

    
    end subroutine ordenar




end module estatistica




