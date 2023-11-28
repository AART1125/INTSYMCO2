from pyswip import Prolog
import re

prolog = Prolog()
prolog.consult("knowledge_base.pl")

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
    r'Is (\w+) an uncle of (\w+)',  # 10
    r'Who are the siblings of (\w+)',  # 11
    r'Who are the sisters of (\w+)',  # 12
    r'Who are the brothers of (\w+)',  # 13
    r'Who is the mother of (\w+)',  # 14
    r'Who is the father of (\w+)',  # 15
    r'Who are the parents of (\w+)',  # 16
    r'Is (\w+) a grandfather of (\w+)',  # 17
    r'Who are the daughters of (\w+)',  # 18
    r'Who are the sons of (\w+)',  # 19
    r'Who are the children of (\w+)',  # 20
    r'Is (\w+) an aunt of (\w+)',  # 21
    r'Are (\w+) and (\w+) relatives'  # 22
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
        query = f"siblings({sibling1}, {sibling2}), siblings({sibling2}, {sibling1})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            prolog.assertz(f"siblings({sibling1}, {sibling2})")
            prolog.assertz(f"siblings({sibling2}, {sibling1})")
            print("Ok, I'll remember that")
    
    if index == 1: # is a sister
        sibling1 = match.group(1)
        sibling2 = match.group(2)
        query = f"sister({sibling1},{sibling2})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"male({sibling1}); parent({sibling2},{sibling1}); parent({sibling1},{sibling2})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"female({sibling1})")
                prolog.assertz(f"sister({sibling1}, {sibling2})")
                prolog.assertz(f"siblings({sibling1}, {sibling2})")
                prolog.assertz(f"siblings({sibling2}, {sibling1})")
                print("Ok, I'll remember that")


    if index == 2:  # is the mother
        mother = match.group(1)
        child = match.group(2)
        query = f"mother({mother},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child}, {mother}); male({mother}); siblings({mother},{child}); (child({child}, Y), female(Y), !)"
            if list(prolog.query(new_query)) or child == mother:
                print("That's not possible")
            else:
                prolog.assertz(f"female({mother})")
                prolog.assertz(f"mother({mother},{child})")
                prolog.assertz(f"child({child},{mother})")
                prolog.assertz(f"parent({mother},{child})")
                print("Ok, I'll remember that")    
    
    if index == 3: #is a grandmother
        grandmother = match.group(1)
        child = match.group(2)
        query = f"grandmother({grandmother},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child},{grandmother}); male({grandmother}); siblings({grandmother},{child})"
            if list(prolog.query(new_query)) or child == grandmother:
                print("That's not possible")
            else:
                prolog.assertz(f"female({grandmother})")
                prolog.assertz(f"grandmother({grandmother},{child})")
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
        query = f"daughter({daughter},{parent})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({daughter},{parent})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"female({daughter})")
                prolog.assertz(f"daughter({daughter},{parent})")
                prolog.assertz(f"child({daughter},{parent})")
                prolog.assertz(f"parent({parent}, {daughter})")
                print("Ok, I'll remember that")

    if index == 6:  # is an uncle
        uncle = match.group(1)
        child = match.group(2)
        query = f"uncle({uncle},{child})" 
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
        query = f"brother({sibling1},{sibling2})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"female({sibling1}); parent({sibling2},{sibling1}); parent({sibling1},{sibling2})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"male({sibling1})")
                prolog.assertz(f"brother({sibling1}, {sibling2})")
                prolog.assertz(f"siblings({sibling1}, {sibling2})")
                prolog.assertz(f"siblings({sibling2}, {sibling1})")
                print("Ok, I'll remember that")

    if index == 8:  # is the father
        father = match.group(1)
        child = match.group(2)
        query = f"father({father},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            existing_father_query = f"parent({child},{father}); female({father}); siblings({father},{child}); (child({child}, Y), male(Y), !)"
            if list(prolog.query(existing_father_query)):
                print(f"That's not possible.")
            else:
                prolog.assertz(f"male({father})")
                prolog.assertz(f"father({father},{child})")
                prolog.assertz(f"child({child},{father})")
                prolog.assertz(f"parent({father},{child})")
                print("Ok, I'll remember that")
                
    if index == 9:  # are the parents
        parent1 = match.group(1)
        parent2 = match.group(2)
        child = match.group(3)
        query = f"parent({parent1},{child}), child({child},{parent1}), parent({parent2},{child}), child({child},{parent2})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            existing_father_query = f"parent({child},{parent1}); siblings({parent1},{child}), parent({child},{parent2}); siblings({parent2},{child}); (child({child}, Y), parent(Y,{child}), !)"
            if list(prolog.query(existing_father_query)):
                print(f"That's not possible.")
            else:
                prolog.assertz(f"child({child},{parent1})")
                prolog.assertz(f"parent({parent1},{child})")
                prolog.assertz(f"child({child},{parent2})")
                prolog.assertz(f"parent({parent2},{child})")
                print("Ok, I'll remember that")
    
    if index == 10: #is a grandfather
        grandfather = match.group(1)
        child = match.group(2)
        query = f"grandfather({grandfather},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({child},{grandfather}); female({grandfather}); siblings({grandfather},{child}); grandparent({child},{grandfather})"
            if list(prolog.query(new_query)) or child == grandfather:
                print("That's not possible")
            else:
                prolog.assertz(f"male({grandfather})")
                prolog.assertz(f"grandfather({grandfather},{child})")
                print("Ok, I'll remember that")
    
    if index == 11:# is a son
        son = match.group(1)
        parent = match.group(2)
        query = f"son({son},{parent})"
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"parent({son},{parent})"
            if list(prolog.query(new_query)):
                print("That's impossible")
            else:
                prolog.assertz(f"male({son})")
                prolog.assertz(f"child({son},{parent})")
                prolog.assertz(f"son({son},{parent})")
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
        
    children = re.findall(r'\b[A-Z][a-z]*\b', sentence)
    if children and index == -1 and "children" in sentence and "Are" in sentence: # are children of
        try:
            children = re.findall(r'\b[A-Z][a-z]*\b', sentence)
            children.pop(0)
            parent = children.pop()
            known = True
            query = ""
            for i in children:
                query += f" child({i.lower()},{parent.lower()}), parent({parent.lower()},{i.lower()}),"
            query = query[:-1] + ""
            if bool(list(prolog.query(query))):
                names = [i for i in children]
                print(f"Yes, the children of {parent.capitalize()} are: {', '.join(names)}")
            else:
                print("No they are not")
        except IndexError:
            print("You might have inputed the wrong type of statement. Please try again")

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
        query = f"sister({sister}, {sibling})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the sister of {sibling.capitalize()} is {sister.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 2: # is a brother?
        brother = match.group(1)
        sibling = match.group(2)
        query = f"brother({brother}, {sibling})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the brother of {sibling.capitalize()} is {brother.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 3: # is the mother?
        mother = match.group(1)
        child = match.group(2)
        query = f"mother({mother},{child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the mother of {child.capitalize()} is {mother.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 4:# is the father?
        father = match.group(1)
        child = match.group(2)
        query = f"father({father}, {child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the father of {child.capitalize()} is {father.capitalize()}")
        else:
            print(f"No, they are not")

    elif index == 5:  # are the parents?
        parent1 = match.group(1)
        parent2 = match.group(2)
        child = match.group(3)

        if parent1.lower() != parent2.lower():
            query = f"parent({parent1},{child}), parent({parent2},{child})"
            exist = bool(list(prolog.query(query)))
            if exist:
                print(f"Yes, {parent1.capitalize()} and {parent2.capitalize()} are the parents of {child.capitalize()}")
            else:
                print(f"No, they are not")
        else:
            print("Please provide distinct parents.")
            
    if index == 6: #is the grandmother?
        grandmother = match.group(1)
        child = match.group(2)
        query = f"grandmother({grandmother}, {child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the grandmother of {child.capitalize()} is {grandmother.capitalize()}")
        else:
            print(f"No, they are not")
            
    if index == 7:# is a daughter?
        daughter = match.group(1)
        parent = match.group(2)
        query = f"daughter({daughter},{parent})"
        exist = bool(list(prolog.query(query)))
        if exist and parent != daughter:
            print(f"Yes, {daughter.capitalize()} is a daughter of {parent.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 8:# is a son?
        son = match.group(1)
        parent = match.group(2)
        query = f"son({son},{parent})"
        exist = bool(list(prolog.query(query)))
        if exist and parent != son:
            print(f"Yes, {son.capitalize()} is a son of {parent.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 9: #is a child?
        child = match.group(1)
        parent = match.group(2)
        query = f"child({child},{parent})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {child.capitalize()} is a child of {parent.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 10: # Is (\w+) an uncle of (\w+)?
        uncle = match.group(1)
        child = match.group(2)

        query = f"uncle({uncle},{child})"
        exist = bool(list(prolog.query(query)))

        if exist:
            print(f"Yes, {uncle.capitalize()} is an uncle of {child.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 11: # Who are the siblings of (\w+)?
        person = match.group(1)

        query = f"siblings({person}, Sibling)"
        siblings = list(prolog.query(query))

        if siblings:
            sibling_names = [s["Sibling"].capitalize() for s in siblings if s["Sibling"] != person.lower()]
            print(f"The siblings of {person.capitalize()} are: {', '.join(sibling_names)}")
        else:
            print(f"{person.capitalize()} has no known siblings.")
    
    if index == 12:  # Who are the sisters of (\w+)?
        sibling = match.group(1)
        query = f"sister(Sister, {sibling.lower()}), Sister \\= {sibling.lower()}"
        exist = list(prolog.query(query))
        if exist:
            sisters = set(s["Sister"].capitalize() for s in exist)
            print(f"The sisters of {sibling.capitalize()} are: {', '.join(sisters)}")
        else:
            print(f"{sibling.capitalize()} has no sisters")

    if index == 13: # Who are the brothers of (\w+)?
        sibling = match.group(1)
        query = f"brother(Brother, {sibling.lower()}), Brother \\= {sibling.lower()}"
        exist = list(prolog.query(query))
        if exist:
            brothers = set(b["Brother"].capitalize() for b in exist)
            print(f"The brothers of {sibling.capitalize()} are: {', '.join(brothers)}")
        else:
            print(f"{sibling.capitalize()} has no brothers")


    if index == 14: # Who is the mother of (\w+)?
        child = match.group(1)
        query = f"mother(X, {child})"
        mothers = list(prolog.query(query))
        if mothers:
            print(f"The mother of {child.capitalize()} is: {mothers[0]['X'].capitalize()}")
        else:
            print(f"I don't know who the mother of {child.capitalize()} is.")

    if index == 15: # Who is the father of (\w+)?
        child = match.group(1)
        query = f"father(X, {child})"
        fathers = list(prolog.query(query))
        if fathers:
            print(f"The father of {child.capitalize()} is: {fathers[0]['X'].capitalize()}")
        else:
            print(f"I don't know who the father of {child.capitalize()} is.")

    if index == 16:  # who are the parents?
        child = match.group(1)
        query = f"parent(Parent, {child.lower()})"
        exist = list(prolog.query(query))
        if exist:
            parents = set(p["Parent"].capitalize() for p in exist)
            print(f"The parents of {child.capitalize()} are: {', '.join(parents)}")
        else:
            print(f"{child.capitalize()} has no known parents")
    
    if index == 17:  # is grandfather?
        grandfather = match.group(1)
        child = match.group(2)
        query = f"grandfather({grandfather.lower()}, {child.lower()})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {grandfather.capitalize()} is a grandfather of {child.capitalize()}")
        else:
            print(f"No, {grandfather.capitalize()} is not a grandfather of {child.capitalize()}")

    if index == 18:# who are the daughters.
        parent = match.group(1)
        query = f"daughter(Daughter,{parent})"
        exist = list(prolog.query(query))
        if exist:
            daughters = set(d["Daughter"].capitalize() for d in exist)
            print(f"The daughters of {parent.capitalize()} are: {', '.join(daughters)}")
        else:
            print(f"{parent.capitalize()} has no daughters")

    if index == 19:# who are the sons.
        parent = match.group(1)
        query = f"son(Son, {parent})"
        exist = list(prolog.query(query))
        if exist:
            sons = set(d["Son"].capitalize() for d in exist)
            print(f"The sons of {parent.capitalize()} are: {', '.join(sons)}")
        else:
            print(f"{parent.capitalize()} has no sons")

    if index == 20:  # Who are the children?
        parent = match.group(1)
        query = f"parent({parent.lower()}, Child), child(Child, {parent.lower()})"
        exist = list(prolog.query(query))
        if exist:
            children = set(c["Child"].capitalize() for c in exist)
            print(f"The children of {parent.capitalize()} are: {', '.join(children)}")
        else:
            print(f"{parent.capitalize()} has no children")

    if index == 21: # is aunt?
        aunt = match.group(1)
        child = match.group(2)

        query = f"aunt({aunt},{child})"
        exist = bool(list(prolog.query(query)))

        if exist:
            print(f"Yes, {aunt.capitalize()} is an aunt of {child.capitalize()}")
        else:
            print(f"No, they are not")

    if index == 22:  # Are relatives?
        person1 = match.group(1)
        person2 = match.group(2)
        query = f"relatives({person1.lower()}, {person2.lower()})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, {person1.capitalize()} and {person2.capitalize()} are relatives.")
        else:
            print(f"No, {person1.capitalize()} and {person2.capitalize()} are not relatives.")

if __name__ == "__main__":
    main()