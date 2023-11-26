from pyswip import Prolog
import re

prolog = Prolog()
prolog.consult("test_kb.pl")

statement_patterns = [
    r'(\w+) and (\w+) are siblings',#0
    r'(\w+) is a sister of (\w+)',#1
    r'(\w+) is the mother of (\w+)',#2
    r'(\w+) is a grandmother of (\w+)',#3
    r'(\w+) is a child of (\w+)',#4
    r'(\w+) is a daughter of (\w+)',#5
    r'(\w+) is an uncle of (\w+)',#6
    r'(\w+) is a brother of (\w+)',#7
    r'(\w+) is the father of (\w+)',#8
    r'(\w+) and (\w+) are the parents of (\w+)',#9
    r'(\w+) is a grandfather of (\w+)',#10
    r'(\w+), (\w+) and (\w+) are children of (\w+)',#11
    r'(\w+) is a son of (\w+)',#12
    r'(\w+) is a aunt of (\w+)',#13
]

question_patterns = [
    r'are (\w+) and (\w+) siblings',#0
    r'is (\w+) a sister of (\w+)',#1
    r'is (\w+) a brother of (\w+)',#2
    r'is (\w+) the mother of (\w+)',#3
    r'is (\w+) the father of (\w+)',#4
    r'are (\w+) and (\w+) the parents of (\w+)',#5
    r'is (\w+) a grandmother of (\w+)',#6
    r'is (\w+) a daughter of (\w+)',#7
    r'is (\w+) a son of (\w+)',#8
    r'is (\w+) a child of (\w+)',#9
    r'are (\w+),(\w+) and (\w+), children of (\w+)',#10
    r'is (\w+) an uncle of (\w+)',#11
    r'who are the siblings of (\w+)',#12
    r'who are the sisters of (\w+)',#13
    r'who are the brothers of (\w+)',#14
    r'who is the mother of (\w+)',#15
    r'who is the father of (\w+)',#16
    r'who are the parents of (\w+)',#17
    r'is (\w+) a grandfather of (\w+)',#18
    r'who are the daughters of (\w+)',#19
    r'who are the sons of (\w+)',#20
    r'who are the children of (\w+)',#21
    r'is (\w+) an aunt of (\w+)',#22
    r'are (\w+) and (\w+) relatives'#23
]

def main():
    print("Hello dear user! What do you want to learn or tell me? (To end chat type END)")
    sentence = input('>> ')
    while True:
        if sentence[-1] == '?':
            question(sentence.lower())
        elif sentence[-1] == '.':
            statement(sentence.lower())
        elif sentence in ['hi', 'hello', 'sup', 'Hi', 'Hello']:
            print("Hello to you too!")
        elif sentence == 'END':
            exit(0)
        else:
            print("Hmmm, I don't get what you mean? Check if you used a valid statement or the right punctuation")
            
        sentence = input('>> ')

def statement(sentence):
    index = -1
    for i in statement_patterns:
        match = re.match(i, sentence)
        if match == None:
            continue
        else:
            index = statement_patterns.index(i)
            break
        
    if index == 0: # are siblings
        child1 = match.group(1)
        child2 = match.group(2)
        query = f"child({child1}), child({child2}), siblings({child1}, {child2}), siblings({child2}, {child1})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            prolog.assertz(f"child({child1})")
            prolog.assertz(f"child({child2})")
            prolog.assertz(f"siblings({child1}, {child2})")
            prolog.assertz(f"siblings({child2}, {child1})")
            print("Ok, I'll remember that")
    
    if index == 2: #is the mother
        mother = match.group(1)
        child = match.group(2)
        query = f"parent({mother},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child}, {mother}); male({mother})"
            if list(prolog.query(new_query)) or child == mother:
                print("That's not possible")
            else:
                prolog.assertz(f"female({mother})")
                prolog.assertz(f"mother({mother})")
                prolog.assertz(f"child({child})")
                prolog.assertz(f"parent({mother},{child})")
                print("Ok, I'll remember that")    
    
    if index == 4: # is a child
        child = match.group(1)
        parent = match.group(2)
        query = f"child({child}), parent({parent}, {child})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            prolog.assertz(f"child({child})")
            prolog.assertz(f"parent({parent}, {child})")
            print("Ok, I'll remember that")
    
    if index == 5: # is a daughter
        daughter = match.group(1)
        parent = match.group(2)
        query = f"female({daughter}), child({daughter}), parent({parent}, {daughter})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            prolog.assertz(f"female({daughter})")
            prolog.assertz(f"child({daughter})")
            prolog.assertz(f"gparents({parent})")
            prolog.assertz(f"parent({parent}, {daughter})")
            print("Ok, I'll remember that")
    
    if index == 8: #is the father
        father = match.group(1)
        child = match.group(2)
        query = f"parent(male({father}), {child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child}, {father}); female({father})"
            if list(prolog.query(new_query)) or child == father:
                print("That's not possible")
            else:
                prolog.assertz(f"male({father})")
                prolog.assertz(f"father({father})")
                prolog.assertz(f"child({child})")
                prolog.assertz(f"parent({father},{child})")
                print("Ok, I'll remember that")
    
    if index == 12:# is a son
        son = match.group(1)
        parent = match.group(2)
        query = f"male({son}), child({son}), parent({parent}, {son})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            prolog.assertz(f"male({son})")
            prolog.assertz(f"child({son})")
            prolog.assertz(f"parent({parent}, {son})")
            print("Ok, I'll remember that")

def question(sentence):
    index = 0
    for i in question_patterns:
        match = re.match(i, sentence)
        if match == None:
            continue
        else:
            index = question_patterns.index(i)
            break
        
    if index == 0:
        child1 = match.group(1)
        child2 = match.group(2)
        query = f"child({child1}), child({child2}), siblings({child1}, {child2}), siblings({child2}, {child1})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {child1.capitalize()} and {child2.capitalize()} are siblings.")
        else:
            print(f"No, they are not")
        
    if index == 3:
        mother = match.group(1)
        child = match.group(2)
        query = f"parent({mother}, {child}), mother({mother}), child({child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the mother of {child.capitalize()} is {mother.capitalize()}")
        else:
            print(f"No, they are not")
        
    if index == 4:
        father = match.group(1)
        child = match.group(2)
        query = f"parent({father}, {child}), father({father}), child({child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the father of {child.capitalize()} is {father.capitalize()}")
        else:
            print(f"No, they are not")
            
    if index == 7:
        daughter = match.group(1)
        parent = match.group(2)
        query = f"female({daughter}), child({daughter}), parent({parent},{daughter})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {daughter.capitalize()} is a daughter of {parent.capitalize()}")
        else:
            print(f"No, they are not")
            
    if index == 9:
        child = match.group(1)
        parent = match.group(2)
        query = f"child({child}), parent({parent},{child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {child.capitalize()} is a child of {parent.capitalize()}")
        else:
            print(f"No, they are not")
            
    if index == 19:
        parent = match.group(1)
        query = f"parent({parent},Y), female(Y)"
        exist = list(prolog.query(query))
        if exist:
            for i in exist:
                print(i["Y"].capitalize())
            print(f"These are the daughters of {parent.capitalize()}")
        else:
            print(f"{parent.capitalize()} has no daughters")

if __name__ == "__main__":
    main()