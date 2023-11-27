from pyswip import Prolog
import re

prolog = Prolog()
prolog.consult("test_kb.pl")

statement_patterns = [
    r'(\w+) and (\w+) are siblings',  # 0
    r'(\w+) is a sister of (\w+)',  # 1
    r'(\w+) is the mother of (\w+)',  # 2
    r'(\w+) is a grandmother of (\w+)',  # 3
    r'(\w+) is a child of (\w+)',  # 4
    r'(\w+) is a daughter of (\w+)',  # 5
    r'(\w+) is an uncle of (\w+)',  # 6
    r'(\w+) is a brother of (\w+)',  # 7
    r'(\w+) is the father of (\w+)',  # 8
    r'(\w+) and (\w+) are the parents of (\w+)',  # 9
    r'(\w+) is a grandfather of (\w+)',  # 10
    r'(\w+) is a son of (\w+)',  # 11
    r'(\w+) is an aunt of (\w+)',  # 12
]

question_patterns = [
    r'Are (\w+) and (\w+) siblings',  # 0
    r'Is (\w+) a sister of (\w+)',  # 1
    r'Is (\w+) a brother of (\w+)',  # 2
    r'Is (\w+) the mother of (\w+)',  # 3
    r'Is (\w+) the father of (\w+)',  # 4
    r'Are (\w+) and (\w+) the parents of (\w+)',  # 5
    r'Is (\w+) a grandmother of (\w+)',  # 6
    r'Is (\w+) a daughter of (\w+)',  # 7
    r'Is (\w+) a son of (\w+)',  # 8
    r'Is (\w+) a child of (\w+)',  # 9
    r'Are (\w+),(\w+) and (\w+), children of (\w+)',  # 10
    r'Is (\w+) an uncle of (\w+)',  # 11
    r'Who are the siblings of (\w+)',  # 12
    r'Who are the sisters of (\w+)',  # 13
    r'Who are the brothers of (\w+)',  # 14
    r'Who is the mother of (\w+)',  # 15
    r'Who is the father of (\w+)',  # 16
    r'Who are the parents of (\w+)',  # 17
    r'Is (\w+) a grandfather of (\w+)',  # 18
    r'Who are the daughters of (\w+)',  # 19
    r'Who are the sons of (\w+)',  # 20
    r'Who are the children of (\w+)',  # 21
    r'Is (\w+) an aunt of (\w+)',  # 22
    r'Are (\w+) and (\w+) relatives'  # 23
]

def main():
    print("Hello dear user! What do you want to learn or tell me? (To end chat type END)")
    sentence = input('>> ')
    while True:
        if sentence[-1] == '?':
            question(sentence)
        elif sentence[-1] == '.':
            statement(sentence)
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
        match = re.match(i, sentence.lower())
        if match == None:
            continue
        else:
            index = statement_patterns.index(i)
            break
        
    children = re.findall(r'\b[A-Z][a-z]*\b', sentence)
    if children and index == -1 and "children" in sentence: # are children of
        try:
            children = re.findall(r'\b[A-Z][a-z]*\b', sentence)
            parent = children.pop()
            known = True
            for i in children:
                query = f"child({i.lower()}, {parent.lower()}), parent({parent.lower()}, {i.lower()})"
                if list(prolog.query(query)):
                    print("I already know that")
                else:
                    print("I did not know that")
                    known = False
            if not known:
                for i in children:
                    prolog.assertz(f"child({i.lower()},{parent.lower()})")
                    prolog.assertz(f"parent({parent.lower()}, {i.lower()})")
                print("Ok, I'll remember that")
        except IndexError:
            print("You might have inputed the wrong type of statement. Please try again")
          
    if index == 0: # are siblings
        sibling1 = match.group(1)
        sibling2 = match.group(2)
        query = f"siblings({sibling1}, {sibling2}), siblings({sibling2}, {sibling1}), \+ uncle({sibling1},{sibling2})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            prolog.assertz(f"siblings({sibling1}, {sibling2})")
            prolog.assertz(f"siblings({sibling2}, {sibling1})")
            print("Ok, I'll remember that")
    
    if index == 1: # is a sister
        sibling1 = match.group(1)
        sibling2 = match.group(2)
        query = f"female({sibling1}), siblings({sibling1}, {sibling2}), siblings({sibling2}, {sibling1})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"male({sibling1}); parent({sibling2},{sibling1}); parent({sibling1},{sibling2})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"female({sibling1})")
                prolog.assertz(f"siblings({sibling1}, {sibling2})")
                prolog.assertz(f"siblings({sibling2}, {sibling1})")
                print("Ok, I'll remember that")


    if index == 2:  # is the mother
        mother = match.group(1)
        child = match.group(2)
        query = f"parent({mother},{child}), child({child},{mother}), mother({mother})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child}, {mother}); male({mother}); (child({child}, Y), female(Y), !); siblings({mother},{child}); grandparent({child},{mother})"
            if list(prolog.query(new_query)) or child == mother:
                print("That's not possible")
            else:
                prolog.assertz(f"female({mother})")
                prolog.assertz(f"mother({mother})")
                prolog.assertz(f"child({child},{mother})")
                prolog.assertz(f"parent({mother},{child})")
                print("Ok, I'll remember that")    
    
    if index == 3: #is a grandmother
        grandmother = match.group(1)
        child = match.group(2)
        query = f"grandparent({grandmother},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child},{grandmother}); male({grandmother}); siblings({grandmother},{child})"
            if list(prolog.query(new_query)) or child == grandmother:
                print("That's not possible")
            else:
                prolog.assertz(f"female({grandmother})")
                prolog.assertz(f"mother({grandmother})")
                prolog.assertz(f"grandparent({grandmother},{child})")
                print("Ok, I'll remember that")
    
    if index == 4: # is a child
        child = match.group(1)
        parent = match.group(2)
        query = f"child({child},{parent}), parent({parent}, {child})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"child({parent},{child}); parent({child},{parent})"
            if list(prolog.query(new_query)):
                print("That's not possible")
            else:
                prolog.assertz(f"child({child},{parent})")
                prolog.assertz(f"parent({parent},{child})")
                print("Ok, I'll remember that")

    if index == 5:  # is a daughter
        daughter = match.group(1)
        parent = match.group(2)
        query = f"female({daughter}), child({daughter},{parent}), parent({parent}, {daughter})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({daughter},{parent})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"female({daughter})")
                prolog.assertz(f"child({daughter},{parent})")
                prolog.assertz(f"parent({parent}, {daughter})")
                print("Ok, I'll remember that")

    if index == 6:  # is an uncle
        uncle = match.group(1)
        child = match.group(2)
        query = f"male({uncle}), uncle({uncle},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child},{uncle}); parent({uncle},{child}); siblings({child},{uncle})"
            if list(prolog.query(new_query)):
                print(f"That's not possible.")
            else:
                prolog.assertz(f"male({uncle})")
                prolog.assertz(f"uncle({uncle},{child})")
                print("Ok, I'll remember that")

    if index == 7: # is a brother
        sibling1 = match.group(1)
        sibling2 = match.group(2)
        query = f"male({sibling1}), siblings({sibling1}, {sibling2}), siblings({sibling2}, {sibling1})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"female({sibling1}); parent({sibling2},{sibling1}); parent({sibling1},{sibling2})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"male({sibling1})")
                prolog.assertz(f"siblings({sibling1}, {sibling2})")
                prolog.assertz(f"siblings({sibling2}, {sibling1})")
                print("Ok, I'll remember that")

    if index == 8:  # is the father
        father = match.group(1)
        child = match.group(2)
        query = f"male({father}), parent({father},{child}), child({child},{father})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            existing_father_query = f"parent({child},{father}); female({father}); siblings({father},{child}); (child({child}, Y), male(Y), !)"
            if list(prolog.query(existing_father_query)):
                print(f"That's not possible.")
            else:
                prolog.assertz(f"male({father})")
                prolog.assertz(f"father({father})")
                prolog.assertz(f"child({child},{father})")
                prolog.assertz(f"parent({father},{child})")
                print("Ok, I'll remember that")
                
    if index == 9:  # is the father
        parent1 = match.group(1)
        parent2 = match.group(2)
        child = match.group(3)
        query = f"parent({parent1},{child}), child({child},{parent1}), parent({parent2},{child}), child({child},{parent2})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            existing_father_query = f"parent({child},{parent1}); siblings({parent1},{child}), parent({child},{parent2}); siblings({parent2},{child})"
            if list(prolog.query(existing_father_query)):
                print(f"That's not possible.")
            else:
                prolog.assertz(f"child({child},{parent1})")
                prolog.assertz(f"parent({parent1},{child})")
                prolog.assertz(f"child({child},{parent2})")
                prolog.assertz(f"parent({parent2},{child})")
                print("Ok, I'll remember that")
    
    if index == 10: #is a grandfater
        grandfather = match.group(1)
        child = match.group(2)
        query = f"grandparent({grandfather},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child},{grandfather}); female({grandfather}); siblings({grandfather},{child}); grandparent({child},{grandfather})"
            if list(prolog.query(new_query)) or child == grandfather:
                print("That's not possible")
            else:
                prolog.assertz(f"male({grandfather})")
                prolog.assertz(f"father({grandfather})")
                prolog.assertz(f"grandparent({grandfather},{child})")
                print("Ok, I'll remember that")
    
    if index == 11:# is a son
        son = match.group(1)
        parent = match.group(2)
        query = f"male({son}), child({son},{parent}), parent({parent}, {son})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({son},{parent})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"male({son})")
                prolog.assertz(f"child({son},{parent})")
                prolog.assertz(f"parent({parent}, {son})")
                print("Ok, I'll remember that")

    if index == 12:  # is an uncle
        aunt = match.group(1)
        child = match.group(2)
        query = f"female({aunt}), aunt({aunt},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child},{aunt}); parent({aunt},{child}); siblings({child},{aunt})"
            if list(prolog.query(new_query)):
                print(f"That's not possible.")
            else:
                prolog.assertz(f"female({aunt})")
                prolog.assertz(f"aunt({aunt},{child})")
                print("Ok, I'll remember that")

def question(sentence):
    index = -1
    for i in question_patterns:
        match = re.match(i, sentence)
        if match == None:
            continue
        else:
            index = question_patterns.index(i)
            break

    if index == 0: # are siblings?
        child1 = match.group(1)
        child2 = match.group(2)
        query = f"siblings({child1}, {child2}), siblings({child2}, {child1})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {child1.capitalize()} and {child2.capitalize()} are siblings.")
        else:
            print(f"No, they are not")

    if index == 1: # is a sister?
        sister = match.group(1)
        sibling = match.group(2)
        query = f"siblings({sister}, {sibling})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the sister of {sibling.capitalize()} is {sister.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 3: # is the mother?
        mother = match.group(1)
        child = match.group(2)
        query = f"parent({mother}, {child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the mother of {child.capitalize()} is {mother.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 4:# is the father?
        father = match.group(1)
        child = match.group(2)
        query = f"parent({father}, {child}), father({father}), child({child}, {father})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the father of {child.capitalize()} is {father.capitalize()}")
        else:
            print(f"No, they are not")
            
    if index == 6: #is the grandmother?
        grandmother = match.group(1)
        child = match.group(2)
        query = f"grandparent({grandmother}, {child}), parent({grandmother}, X), parent(X, {child}), child({child},X)"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the grandmother of {child.capitalize()} is {grandmother.capitalize()}")
        else:
            print(f"No, they are not")
            
    if index == 7:# is a daughter?
        daughter = match.group(1)
        parent = match.group(2)
        query = f"child_of({parent},{daughter})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {daughter.capitalize()} is a daughter of {parent.capitalize()}")
        else:
            print(f"No, {daughter.capitalize()} is not a daughter of {parent.capitalize()}")



    if index == 9: #is a child?
        child = match.group(1)
        parent = match.group(2)
        query = f"child_of({parent},{child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {child.capitalize()} is a child of {parent.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 19:# who are the daughters.
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
