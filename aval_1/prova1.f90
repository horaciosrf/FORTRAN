program prova
    implicit none
!---------------CABEÇALHO-------------------------------------------------
    !Primeira avaliação da disciplina de Fortran do CPGF
    !Professor: Cícero Roberto Teixeira Régis
    !Programador aluno: Horácio Santiago Ribeiro Ferreira
    !Data: 25/05/2021 
!---------------PROBLEMA--------------------------------------------------
    !Problema: Lançamento de projeteis. A equação que define
    !o tempo da trajetoria é relacionada a um lançamento
    !obliquo. Para tal temos que o tempo seria dada pela
    !equação do 2 grau: g*t^2 - 2*Vo*sen(theta)*t - 2*h = 0 . 
    !Tendo também em vista que a distancia x será:
    !x = Vo*cos(theta)*t. 
    !Com isto devemos achar t e x.

!---------------VARIÁVEIS-------------------------------------------------
    !Declaração de variáveis:
    real,parameter    :: g = 9.8 !Aceleraçaõ da gravidade (m/s^2)
    real,parameter    :: pi = 3.1415926 !Valor de pi
    real              :: grau2rad !Conversão de grau pra radiano
    real              :: h, v, theta, the !Variaveis de entrada
    real              :: t, x !Variaveis de saída
    real              :: a,b,c,t1,t2 !Variaveis para a subrotina
    real,parameter    :: eps = 1.e-5 !Parametro de teste de precisão

!---------------PROGRAMA--------------------------------------------------
    grau2rad = pi/180. !definimos a variável de conversão de grau pra rad
    !Vamos entrar com as variaveis de entrada
    write(*,*)'Esse programa calcula o tempo e distancia de um tiro de morteiro'
    write(*,*)'Entre com os valores de:'
    write(*,*)'Altura inicial (m):'
    read(*,*) h !Entrando com a variável da altura inicial
    write(*,*)'Velocidade inicial (m/s):'
    read(*,*) v !Entrando com a variável da velocidade inicial
    write(*,*)'Angulo do tiro (graus):'
    read(*,*) theta !Entrando com o ângulo de lançamento

    
    !Precisamos testar se o angulo é menor que 90 graus e maior que -90
    !A necessidade disso é visto que a equação do lançamento analisada 
    !é preposta nos quadrantes designados de -90 a 90, que é onde o x
    !vai ser maior ou igual a 0, assim como o t. E o que queremos é
    !justamente uma distancia positiva para um tempo positivo. 
    !A influencia de um angulo negativo no seno vai mudar apenas a 
    !velocidade vertical (Vo * sen(theta)), onde ao ser positivo 
    !vai influenciar num lançamento mais demorado e ao ser negativo
    ! a velocidade será negativa e em direção ao chão, o que fará que
    !o lançamento seja mais rápido.
    !Ja a influencia de um angulo negativo no cosseno será insignificante
    !visto que por ser uma função par, cos(theta) = cos(-theta), e isso
    !é visto fisicamente pois o cosseno define a velocidade horizontal
    !(Vo * cos(theta)) a qual não é influenciada pois sempre se manterá
    !positiva enquanto tiver na direção e sentido do eixo x positivo.
    do 
       if ( abs(theta) > 90. ) then
            write(*,*)'O angulo deve estar entre -90 e 90!!'
            write(*,*) 'Entre com um angulo valido em graus:'
            read(*,*) theta
       else if ( abs(theta) <= 90. ) then
            !Com o angulo certo temos que converter para radianos ja que
            !as funções sin e cos só aceitam radianos
            the = theta * grau2rad
            exit
       end if 
    end do
    
    !Outra análise importante também é que a altura não pode ser negativa,
    !senão será necessário corrigir o valor da altura.

    do 
       if ( h < 0. ) then
            write(*,*)'A altura inicial nao pode ser negativa!'
            write(*,*) 'Entre com uma altura maior ou igual a 0:'
            read(*,*) h 
       else
            exit
       end if 
    end do

    !Além disso, fazemos a mesma análise para a velocidade inicial,
    !pois ela não pode ser negativa.
    do 
        if ( v < 0. ) then
             write(*,*)'A velocidade inicial nao pode ser negativa!'
             write(*,*) 'Entre com uma velocidade maior ou igual a 0:'
             read(*,*) v 
        else
             exit
        end if 
     end do
     
    !Agora precisamos definir os coeficientes de segundo grau da equação
    !do tempo

    a = g !O coeficiente a será igual ao g
    b = -2. * v * sin(the) !O coeficiente b será: - 2*Vo*sen(theta)
    c = -2. * h !O coeficiente c será: - 2*h

    !Agora vamos chamar a subrotina que nos dará as raizes da equação

    call quadratic(a,b,c,t1,t2) !t1 e t2 serão as raizes

    
    !Uma das raizes será negativa, então temos que descartá-la
    !Também temos que descartar um tempo igual a 0, que provavelmente
    !vai aparecer caso a altura escolhida seja igual a 0

    if ( t1 <= 0. ) then
        t = t2
    else 
        t = t1 
    end if

    !Agora que temos o tempo t, vamos calcular a distância x

    x = v * cos(the) * t
    
    !É necessário tambem analisar a precisão da equação de x:
    !No caso de angulos como -90 e 90, por conta da precisão do grau2rad,
    !os valores não serão exatamente cos(-90)=0 e cos(90)=0, o que prejudica
    !a conta. nesse caso colocaremos diretamente o x = 0, pois sabemos
    !que não haverá movimento horizontal, só vertical.
    !Para isso, foi adicionado um parametro de precisão em 1.e-6, para
    !analisar os arredores de 90 e -90 graus.
    !Essa precisão foi definida com base nas casa decimais que valores com
    !cos(90) e cos(-90) originalmente estavam chegando, que é a mesma precisão do pi
    
    if ( abs(x) < eps ) then
        x = 0.
    end if

    !Assim, tendo x e t, damos os valores de saida para o terminal
    write(*,*)'Tempo ate o impacto (em segundos):', t
    write(*,*)'Distancia ate o impacto (em metros):', x


!---------------FUNÇÕES-E-SUBROTINAS--------------------------------------
    contains
    subroutine quadratic(a,b,c,r1,r2)
        !Subrotina que calcula as raizes reais da equação ax^2 + bx + c = 0.
        !Dado os coeficientes a,b,c, a subrotina nos retribui com as raizes
        !r1 e r2
        implicit none
        real, intent(in)       :: a, b, c !Coeficientes da equação
        real, intent(out)      :: r1, r2  !Raizes da equação
        real                   :: delta   !Discriminante

        delta = (b**2) - (4.*a*c)
        r1 = (-b + sqrt(delta))/(2.*a) 
        r2 = (-b - sqrt(delta))/(2.*a)
  
    end subroutine quadratic


end program prova