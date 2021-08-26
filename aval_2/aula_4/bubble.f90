program bubble
    implicit none
    ! programa para ordenar os valores lidos de um arquivo
    ! problema: programa pra calcular mediana
    ! Programador: Horácio Santiago Ribeiro Ferreira
    ! Data: 05/06/2021 
    ! real         :: m, dp
    integer          :: N, i
    real,allocatable ::v(:)
    real             :: mn
    ! real            :: t1,t2

    

    open(1,file='dados.dat', status='old', action='read')
    
    read(1,*)N    
    allocate( v(N) )

    do i = 1, N
        read(1,*) v(i)        
    end do

    close(1)

    call mediana(N,v,mn)
    ! call cpu_time(t1)
    call ordenar(v,N)
    ! call cpu_time(t2)

    ! write(*,*)'tempo:',t2-t1

    write(*,*)'Mediana:', mn

    open(2,file='output.dat', status='replace', action='write')

    do i = 1, N
        write(2,*) v(i)        
    end do
    close(2)


    

    contains

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
        
    
end program bubble