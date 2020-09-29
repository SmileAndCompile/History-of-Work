function output = convertfrombase ( str, n ) ;
% This function converts the string str from base n into decimal.
% n must be in the range 2-16

% check base is in range
if n < 2 || n > 16,
    error ( 'Base is out of range! must be 2-16' ) ;
end

output = 0 ; % initialise output to 0

% A few ways to do this.  But the first step is always to convert the
% character form into numerical form for each digit, and store these in a
% digit array

digit = zeros(size(str)) ;

for k = 1:length(str),
    % first check if it is an actual digit, if so just convert to a number
    if str(k) >= '0' && str(k) <= '9',
        digit(k) = str(k) - '0' ;
    elseif str(k) >= 'A' && str(k) <= 'F',
        % capital letter, convert to hex value (A = 10, B = 11, etc..)
        digit(k) = str(k)-'A' + 10 ;
    elseif str(k) >= 'a' && str(k) <= 'f',
        % same for lower case
        digit(k) = str(k)-'a' + 10 ;
    else
        error ( 'Invalid input character' ) ;
    end
    % now check that this number is < n, else it is also invalid
    if digit(k) >= n,
        error ( 'Digit exceeds base-1!' ) ;
    end
end

% now we have an array of digits, we need an array of multipliers to
% calculate the final value.  The rightmost digit is multiplied by n^0,
% next by n^1, n^2, etc..

pow = [length(str)-1:-1:0] ;
mult = n.^pow ;

% now multiply, and simply add them up for output!
output = sum ( mult.*digit ) ;
