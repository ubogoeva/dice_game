import numpy as np
from itertools import permutations as perm
from itertools import combinations as comb


hand_my = np.array([0,0,0,0,0])
hand_op = np.array([0,0,0,0,0])

tab_bid = [[0,1,1,1,2],[0,0,0,1,1]]

count_my = 5
count_op = 5

win = [0,0,0]
win_h = [0,0,0]

def calc_bid(temp): #temp == count of each die
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
    if prev_bid == [max_bid[0], max_bid[1] + count_op]:
        return 0
    bid = next_bid(prev_bid, max_bid[0])
    if bid[1] <= max_bid[1] + count_op:
        return bid
    return 0

def god(max_bid, prev_bid):
    if prev_bid == 0:
        return [max_bid[0],1]
    if prev_bid == [max_bid[0], max_bid[1] + tab_bid[int(max_bid[0]==1)][count_op-1]]:
        return 0
    bid = next_bid(prev_bid, max_bid[0])
    if bid[1] <= max_bid[1] + tab_bid[int(max_bid[0]==1)][count_op-1]:
        return bid
    return 0

def check_win(gamer1, gamer2, max_bid1, max_bid2, hand1=[0,0,0,0,0,0], hand2=[0,0,0,0,0,0], permut=1):

    bid = 0
    prev_bid = 0
    fin = 0
    while(1):
        prev_bid = bid
        bid = gamer1(max_bid1, bid)
        #print("g1", bid)
        if bid == 0:
            fin = 1
            break
        prev_bid = bid
        bid = gamer2(max_bid2, bid)
        #print("g2", bid)
        if bid == 0:
            fin = 2
            break
    #print(prev_bid, max_bid1, max_bid2)
    if np.sum(hand1) != 0:
        w1 = prev_bid[1] - hand1[0] - hand2[0]
        if prev_bid[0] != 1:
            w1 -= hand1[prev_bid[0]-1] + hand2[prev_bid[0]-1]
        if w1 > 0:
            win[fin] += permut
        else:
            win[3-fin] += permut
    else:
        w2 = prev_bid[1]
        for i in hand_my:
            if (i == prev_bid[0]) or (i == 1):
                w2 -= 1
        for i in hand_op:
            if (i == prev_bid[0]) or (i == 1):
                w2 -= 1
        if w2 > 0:
            win_h[fin] += permut
        else:
            win_h[3-fin] += permut
    
        
def er_check_all_hands(gamer1, gamer2):
    temp1 = [0,0,0,0,0,0]
    temp2 = [0,0,0,0,0,0]
    for one in range(1,7):
        temp1[one-1] += 1
        print(one)
        for two in range(1,7):
            temp1[two-1] += 1
            print('\t',two)
            for three in range(1,7):
                temp1[three-1] += 1
                for four in range(1,7):
                    temp1[four-1] += 1
                    for five in range(1,7):
                        temp1[five-1] += 1
                        hand_my = [one,two,three,four,five]
                        max_bid1 = calc_bid(temp1)
                        #print('\t'+str(five))
                        for one_ in range(1,7):
                            temp2[one_-1] += 1
                            for two_ in range(1,7):
                                temp2[two_-1] += 1
                                for three_ in range(1,7):
                                    temp2[three_-1] += 1
                                    for four_ in range(1,7):
                                        temp2[four_-1] += 1
                                        for five_ in range(1,7):
                                            temp2[five_-1] += 1
                                            hand_op = [one_,two_,three_,four_,five_]
                                            max_bid2 = calc_bid(temp2)
                                            check_win(gamer1,gamer2,max_bid1,max_bid2)
                                            temp2[five_-1] -= 1
                                        temp2[four_-1] -= 1
                                    temp2[three_-1] -= 1
                                temp2[two_-1] -= 1
                            temp2[one_-1] -= 1
                        temp1[five-1] -= 1
                    temp1[four-1] -= 1
                temp1[three-1] -= 1
            temp1[two-1] -= 1
        temp1[one-1] -= 1

'''
def check_all_hands(gamer1, gamer2):
    temp1 = [0,0,0,0,0,0]
    temp2 = [0,0,0,0,0,0]
    print("start")
    print()
    for hand_my in comb([1,2,3],5):
        print(hand_my)
        for i in hand_my:
            temp1[i-1] +=1
        max_bid1 = calc_bid(temp1)
        for hand_op in comb([1,2,3],5):
            for i in hand_my:
                temp2[i-1] +=1
            max_bid2 = calc_bid(temp2)
            check_win(gamer1,gamer2,max_bid1,max_bid2)
''' 

def new_check_all_hands(gamer1, gamer2):
    hand1 = [0,0,0,0,0,0]
    hand2 = [0,0,0,0,0,0]
    for one in range(6):
        #print(one)
        for two in range(6-one):
            #print('\t',two)
            for three in range(6-one-two):
                for four in range(6-one-two-three):
                    for five in range(6-one-two-three-four):
                        for six in [5-one-two-three-four-five]:
                            hand1 = [one,two,three,four,five,six]
                            max_bid1 = calc_bid(hand1)
                            x = np.array([])
                            nor_x = 1
                            for i in range(6):
                                x= np.append(x, [i+1]*hand1[i])
                                for f in range(1,hand1[i]+1):
                                    nor_x *= f
                            permut1 = len(list(perm(x)))/nor_x
                            for one_ in range(6):
                                for two_ in range(6-one_):
                                    for three_ in range(6-one_-two_):
                                        for four_ in range(6-one_-two_-three_):
                                            for five_ in range(6-one_-two_-three_-four_):
                                                for six_ in [5-one_-two_-three_-four_-five_]:
                                                    hand2 = [one_,two_,three_,four_,five_,six_]
                                                    max_bid2 = calc_bid(hand2)
                                                    if np.sum(hand1) != 5 or np.sum(hand2) != 5:
                                                        print("error in", hand1, hand2)
                                                        return 0
                                                    y = np.array([])
                                                    nor_y = 1
                                                    for i in range(6):
                                                        y= np.append(y, [i+1]*hand2[i])
                                                        for f in range(1,hand2[i]+1):
                                                            nor_y *= f
                                                    permut = permut1*len(list(perm(y)))/nor_y
                                                    #print(permut)
                                                    check_win(gamer1,gamer2,max_bid1,max_bid2,hand1=hand1,hand2=hand2, permut=permut)
                                               
#hand_my = [4,2,6,6,2]
#hand_op = [1,1,1,4,4]
                                                    
#на данный момент есть три компа: optim, pessim, god                                
new_check_all_hands(pessim, optim) #gamer1, gamer2

#print("hand_opt",hand_my,"hand_pes",hand_op)
#check_win(god,pessim,calc_bid([0,2,0,1,0,2]),calc_bid([3,0,0,2,0,0]),hand1=[0,2,0,1,0,2],hand2=[3,0,0,2,0,0])
print("Pessimist is win " + str(win[1]) + "\t" + str(1.0*win[1]/(win[1]+win[2])))
print("Optimist is win " + str(win[2]) + "\t" + str(1.0*win[2]/(win[1]+win[2])))

#check_all_hands(pessim, optim)
#print("God is win_h " + str(win_h[1]) + "\t" + str(1.0*win_h[1]/(win_h[1]+win_h[2])))
#print("Pessimist is win_h " + str(win_h[2]) + "\t" + str(1.0*win_h[2]/(win_h[1]+win_h[2])))
                        
           
