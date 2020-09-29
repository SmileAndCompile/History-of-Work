fprintf('\nWelcome to Blackjack!\n\n');

money = 10000;
answer = 'Y';

disp('You have $10,000 to play with, with a minimum bet of $500.');
fprintf('Bets must be done in increments of $500.\n\n');


while answer == 'Y' || answer == 'y'
    
bet = input('Please Enter Bet: $');
dhand = [];
phand = [];
dsum = 0;
psum = 0;
dace = 0;
pace = 0;

    while bet > money || rem(bet,500) ~= 0 || bet < 0
        if bet > money
            fprintf('You don''t have enough money to bet that high! you have $%i.\n', money);
            bet = input('Please Enter Bet: $');
        else if bet < 0
                 disp('You can''t bet a negative amount!');
                 bet = input('Please Enter Bet: $');
             else if rem(bet,500) ~= 0
                     disp('Bets must be done in increments of $500!');
                     bet = input('Please Enter Bet: $');
                 end
            end
        end
    end
    
[a1,a2,a3] = card();
if a1 == 'A'
   dace = dace + 1;
end
[b1,b2,b3] = card();
if b1 == 'A'
    pace = pace + 1;
end
[c1,c2,c3] = card();
if c1 == 'A'
    pace = pace + 1;
end

dhand = [dhand, [a1 a2]];
phand = [phand, [b1 b2],[' '], [c1 c2]];
dsum = dsum + a3;
psum = psum + b3 + c3;
dcount = 1;
pcount = 2;

fprintf('Dealer Hand:[ %s ] Value: %i\n', dhand, dsum); 
fprintf('Player Hand:[ %s ] Value: %i\n', phand, psum);

if psum < 21
draw = input('Hit? (Y/N): ','s');
end

if psum > 21 && pace > 0
    psum = psum - 10;
    pace = pace - 1;
end

while psum < 21 && dsum < 21 && pcount < 5 && (draw == 'Y' || draw == 'y')
       [a1,a2,a3] = card();
       pcount = pcount + 1;
       if a1 == 'A'
           pace = pace + 1;
       end
       if dsum < psum && psum < 21
       [b1,b2,b3] = card();
       dcount = dcount + 1;
            if b1 == 'A'
                dace = dace + 1;
            end
       dhand = [dhand,[' '],[b1 b2]];
       dsum = dsum + b3;
       end
       
       phand = [phand,[' '],[a1 a2]];
       psum = psum + a3;
       
       if psum > 21 && pace > 0
           psum = psum - 10;
           pace = pace - 1;
       end
       
       if dsum > 21 && dace > 0
           dsum = dsum - 10;
           dace = dace - 1;
       end
       
       fprintf('Dealer Hand:[ %s ] Value: %i\n', dhand, dsum); 
       fprintf('Player Hand:[ %s ] Value: %i\n', phand, psum);
       
       if psum < 21 && dsum < 21 && pcount < 5
           draw = input('Hit? (Y/N): ','s');
       end
end

while dsum < psum && dsum < 21 && dcount < 5 && psum < 21 && pcount < 5
    disp('(Dealer Drawing Card)');
    pause(2);
    [b1,b2,b3] = card();
    dcount = dcount + 1;
    if b1 == 'A'
        dace = dace + 1;
    end
    dhand = [dhand,[' '],[b1 b2]];
    dsum = dsum + b3;
    if dsum > 21 && dace > 0
        dsum = dsum - 10;
        dace = dace - 1;
    end
    fprintf('Dealer Hand:[ %s ] Value: %i\n', dhand, dsum); 
    fprintf('Player Hand:[ %s ] Value: %i\n', phand, psum);
end


    
if psum > 21
    fprintf('You Lose (Player is Bust) You lost $%i.\n', bet);
    money = money - bet;
elseif dsum > 21
    fprintf('You Win! (Dealer is Bust) You won $%i!\n', bet);
    money = money + bet;    
elseif psum == 21
    fprintf('Blackjack! You Won $%i!\n', bet);
    money = money + bet;
elseif dsum == 21
        fprintf('You Lose (Dealer has Blackjack) You lost $%i.\n', bet);
        money = money - bet;
elseif pcount == 5
        fprintf('You Win! (Card Count = 5) You won $%i!\n', bet);
        money = money + bet;
elseif dcount == 5
        fprintf('You Lose (Dealer Card Count = 5) You lost $%i.\n', bet);
elseif dsum > psum
    fprintf('You Lose (Dealers Hand has Greater Value) You lost $%i.\n', bet);
    money = money - bet; 
elseif dsum == psum 
end

if money >= 500
fprintf('You have $%i, Would you Like to Play Another Hand? (Y/N): ', money);
answer = input('', 's');
else
    answer = 'N';
end

while answer ~= 'Y' && answer ~= 'y' && answer ~= 'N' && answer ~= 'n'
        disp('Error: Incorrect Response. Please Respond with Y/N');
        fprintf('You have $%i, Would you Like to Play Another Hand? (Y/N): ', money);
        answer = input('', 's');
end

end

if money > 10000
    winnings = money - 10000;
    fprintf('Thanks for Playing! You Won $%i!\n\n', winnings);
elseif money < 10000
    loss = 10000 - money;
    fprintf('Thanks for Playing! You Lost $%i.\n\n', loss);
end

