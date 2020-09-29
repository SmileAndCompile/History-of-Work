% main program for Fibonacci numbers

n = input ( 'How many Fibonacci numbers do you want to calculate: ' ) ;
f = fib ( n ) ;
fprintf ( 'The first %i Fibonacci numbers are: ', n ) ;
for k = 1:n,
    if k ~= 1,
        fprintf ( ', ' ) ; % put a comma there to make it look pretty
    end
    fprintf ( '%i', f(k)) ;
end
fprintf ( '\n' ) ;
