function output = fib ( n ) 
% This function calculates the first N Fibonacci numbers

for k = 1:n,
    % loop up to n times
    % first, if k is 1 or 2, the output is 1.
    if k == 1 || k == 2,
        output(k) = 1 ;
    else
        % otherwise, it is the sum of the previous TWO outputs
        output(k) = output(k-1)+output(k-2) ;
    end
end

% all done!
