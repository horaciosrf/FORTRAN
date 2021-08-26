program distancia
    implicit none
!Programa para calcular distancia
!Programador: Horácio Santiago Ribeiro Ferreira
!Data: 27/04/2021

real   :: x1, x2, y1, y2  !coordenadas do ponto 1 e do ponto 2
real   :: d               !distancia


write(*,*) 'coordenadas (x,y) do ponto 1'
read(*,*) x1, y1

write(*,*) 'coordenadas (x,y) do ponto 2'
read(*,*) x2, y2

d= sqrt( (x2-x1)**2 - (y2-y1)**2 )

write(*,*) 'distancia:', d

end program distancia