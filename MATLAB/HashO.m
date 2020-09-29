num = input('Please Enter Row Size: ');
num2 = input('Please Enter Column Size: ');
a = 0;

for c = 1:num
    for b = 1:num2
        if rem(a,2) == 0
            fprintf('#');
        elseif rem(a,2) == 1
            fprintf('0');
        end
        a = a + 1;
    end
    fprintf('\n');
    if rem(num2, 2) == 0
        a = a + 1;
    end
end

            