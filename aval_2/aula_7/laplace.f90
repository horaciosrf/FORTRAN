program laplace2d
    implicit none
    !Programa para resolver a eq de laplace 2d
    !Programador: Horácio Santiago Ribeiro Ferreira
    !Data: 15/06/2021
    integer            :: N
    real, allocatable  ::fi(:,:)
    real               ::f1,f2,f3,f4
    real               :: eps
    real               :: aux, maior, dif
    integer            ::i,j,k
    !integer            ::Niter

    open(1, file='input.dat', status='old', action='read')
    read(1,*)N
    allocate(fi(N,N))
    read(1,*)f1,f2,f3,f4
    read(1,*)eps
    close(1)

    fi = (f1+f2+f3+f4)/4.

    fi(:,1) = f1
    fi(1,:) = f2
    fi(:,N) = f3
    fi(N,:) = f4
    fi(1,1) = (f1+f2)/2.
    fi(1,N) = (f2+f3)/2.
    fi(N,1) = (f4+f1)/2.
    fi(N,N) = (f3+f4)/2.

    k = 0
    do 

        maior = 0.
        do j = 2, N-1
            do i = 2, N-1
                aux = (fi(i-1,j)+fi(i+1,j)+fi(i,j-1)+fi(i,j+1))/4.
                dif = abs(aux-fi(i,j))
                if ( dif > maior ) maior = dif
                    
                fi(i,j) = aux
            end do
        end do
        k = k+1
        if ( maior < eps ) exit 
    end do
    
    write(*,*)'numero de iteracoes:',k

    open(2, file='output.dat', status='replace', action='write')

    do i = 1, N
        write(2,*) (fi(i,j), j=1,N)
    end do
    close(2)
    
end program laplace2d