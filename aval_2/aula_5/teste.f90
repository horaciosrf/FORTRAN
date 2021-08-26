program teste
    implicit none
    ! programa para exemplificar matrizes
    ! problema: produto de uma  matriz MxN por um vetor N
    ! Programador: Horácio Santiago Ribeiro Ferreira
    ! Data: 08/06/2021 
    integer :: M,N,P,i
    real, allocatable    :: A(:,:), B(:,:), C(:,:)

    M = 4
    N = 4
    P = 3
    allocate(A(M,N),B(N,P),C(M,P))
    A(1,:) = [1.,0.,1.,0.]
    A(2,:) = [2.,1.,0.,1.]
    A(3,:) = [3.,0.,2.,1.]
    A(4,:) = [4.,1.,1.,0.]
    B(1,:) = [1.,2.,1.]
    B(2,:) = [1.,1.,0.]
    B(3,:) = [1.,2.,0.]
    B(4,:) = [1.,1.,1.]


    !x=1.

    write(*,*)'Matriz A:'

    do i = 1, M
        write(*,*) A(i,:)        
    end do

    write(*,*)'Matriz B:'
    do i = 1, N
        write(*,*) B(i,:)  
    end do

    !call pro_mat_vet(M,N,A,x,y)
    call pro_mat_mat(M,N,P,A,B,C)
!
    write(*,*)'Matriz C=AB :'
    do i =1,M
        write(*,*)C(i,:)
    end do

    contains

    subroutine pro_mat_vet(M,N,A,x,y)
        implicit none
        integer,intent(in)  :: M,N
        real, intent(out)   :: y(M)
        real,intent(in)     :: A(M,N) , x(N)
        integer             :: i,j
        y = 0.
        do i = 1, M
            do j = 1, N
                y(i) = y(i) + A(i,j) * x(j)                
            end do
        end do

    
    end subroutine pro_mat_vet

    subroutine pro_mat_mat(M,N,P,A,B,C)
        implicit none
        integer,intent(in)  :: M,N,P
        real, intent(out)   :: C(M,P)
        real,intent(in)     :: A(M,N) , B(N,P) 
        integer             :: i,j,k
        C=0.
        do i = 1, M
            do j = 1, P
                do k = 1, N
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do           
            end do
        end do

    
    end subroutine pro_mat_mat

end program teste