import numpy as np

hand_my = np.array([0,0,0,0,0])
hand_op = np.array([0,0,0,0,0])

count_my = 5
count_op = 5

win = [0,0,0]


def calc_bid(hand):
    temp = np.array([0,0,0,0,0,0])
    now = 0
    res = [0,0]
    for i in hand:
        temp[i-1] = temp[i-1] + 1
    for die, k in enumerate(temp):
        if die == 0:
            res = [1, k]
        else:
            if res[0] == 1:
                if k + temp[0] >= res[1]*2 + 1:
                    res = [die + 1, k + temp[0]]
            else:
                if k + temp[0] >= res[1]:
                    res = [die + 1, k + temp[0]]
    #print("res")
    return res
                        
def next_bid(prev_bid, die): #bid[0] == die, bid[1] == count
    res = [0,0]
    if prev_bid[0] == 1:
        if die == 1:
            res = [die, prev_bid[1] + 1]
        else:
            res = [die, prev_bid[1]*2 + 1]
    else:
        if die > prev_bid[0]:
            res = [die, prev_bid[1]]
        else:
            if die == 1:
                res = [die, (prev_bid[1]+1)//2]
            else:
                res = [die, prev_bid[1]+1]
    return res

def pessim(max_bid, prev_bid):
    if prev_bid == 0:
        return [max_bid[0],1]
    if prev_bid == max_bid:
        return [max_bid[0], max_bid[1]+1]
    bid = next_bid(prev_bid, max_bid[0])
    if bid[1] <= max_bid[1]:
        return bid
    return 0

def optim(max_bid, prev_bid):
    if prev_bid == 0:
        return [max_bid[0],1]
    if prev_bid == max_bid:
        return [max_bid[0], max_bid[1]+1]
    bid = next_bid(prev_bid, max_bid[0])
    if bid[1] <= max_bid[1] + count_op:
        return bid
    return 0
def check_win(gamer1, gamer2, max_bid1, max_bid2):

    bid = 0
    prev_bid = 0
    fin = 0
    while(1):
        prev_bid = bid
        bid = gamer2(max_bid2, bid)
        print("pes", bid)
        if bid == 0:
            fin = 2
            break
        prev_bid = bid
        bid = gamer1(max_bid1, bid)
        print("opt", bid)
        if bid == 0:
            fin = 1
            break
    print(prev_bid, max_bid1, max_bid2)
    for i in hand_my:
        if (i == prev_bid[0]) or (i == 1):
            prev_bid[1] = prev_bid[1] - 1
    for i in hand_op:
        if (i == prev_bid[0]) or (i == 1):
            prev_bid[1] = prev_bid[1] - 1
    if prev_bid[1] > 0:
        win[fin] = win[fin] + 1
    else:
        win[3-fin] = win[3-fin] + 1
        
def check_all_hands(gamer1, gamer2):
    for one in range(1,7):
        print(one)
        for two in range(1,7):
            print('\t',two)
            for three in range(1,7):
                for four in range(1,7):
                    for five in range(1,7):
                        hand_my = [one,two,three,four,five]
                        max_bid1 = calc_bid(hand_my)
                        #print('\t'+str(five))
                        for one_ in range(1,7):
                            for two_ in range(1,7):
                                for three_ in range(1,7):
                                    for four_ in range(1,7):
                                        for five_ in range(1,7):
                                            hand_op = [one_,two_,three_,four_,five_]
                                            max_bid2 = calc_bid(hand_op)
                                            check_win(gamer1,gamer2,max_bid1,max_bid2)
                        

#check_all_hands(optim, pessim)
hand_my = [4,2,6,6,2]
hand_op = [2,3,1,4,4]
check_win(optim,pessim,calc_bid(hand_my),calc_bid(hand_op))
print("Pessimist is win " + str(win[2]) + "\t" + str(1.0*win[2]/(win[1]+win[2])))
print("Optimist is win " + str(win[1]) + "\t" + str(1.0*win[1]/(win[1]+win[2])))
                        
           
