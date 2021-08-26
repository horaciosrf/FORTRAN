program teste
    use estatistica
    implicit none
    ! programa para testar o modulo
    ! problema: 
    ! Programador: Horácio Santiago Ribeiro Ferreira
    ! Data: 10/06/2021 
    real, allocatable   :: v(:) 
    integer             :: N,i
    real                :: mn, m, dp


    open(22,file='dados.dat', status='old',action='read')
    read(22,*) N
    allocate(v(N))
    do i = 1, N
        read(22,*) v(i)
    end do
    close(22)

    call calcula_media_dp(N, v, m, dp)
    call mediana(N,v,mn)

    write(*,*)'Media = ',m
    write(*,*)'Desvio padrao = ', dp
    write(*,*)'Mediana = ',mn
end program teste