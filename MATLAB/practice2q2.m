% Practice test 2 q2

% first choose function
func = menu ( 'Which trig function do you want to plot?', 'sin', 'cos', 'tan' ) ;
A = input ( 'Enter amplitude: ' ) ;
f = input ( 'Enter frequency: ' ) ;
xmin = input ( 'Enter starting x value: ' ) ;
xmax = input ( 'Enter final x value: ' ) ;

x = linspace ( xmin, xmax, 500 ) ;

if func == 1,
    y = A*sin(2*pi*f*x) ;
elseif func == 2,
    y = A*cos(2*pi*f*x ) ;
elseif func == 3,
    y = A*tan(2*pi*f*x ) ;
else
    error ( 'something went wrong!' ) ;
end

plot ( x, y ) ;
title ( 'Trig function' ) ;
xlabel ( 'x' ) ;
ylabel ( 'y' ) ;

    