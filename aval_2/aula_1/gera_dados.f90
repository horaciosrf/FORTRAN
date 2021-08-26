program geradados
    implicit none
    real  :: x
    integer :: i,N

    open(33, file='dados.dat', status='replace',action='write')

    write(*,*)'Quantos numeros?'
    read(*,*)N

    write(33,*) N
    do i = 1, N
        call random_number(x)
        x = 2*(x+0.5)
        write(33,*) x
        
    end do

end program geradados