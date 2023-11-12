from pyswip import *
import re

prolog = Prolog()
prolog.consult("knowledge_base.pl", True)

statement_patterns = [
    r'(\w+) and (\w+) are siblings.',
    r'(\w+) is a sister of (\w+)',
    r'(\w+) is the mother of (\w+)',
    r'(\w+) is a grandmother of (\w+)',
    r'(\w+) is a child of (\w+)',
    r'(\w+) is a daughter of (\w+)',
    r'(\w+) is an uncle of (\w+)',
    r'(\w+) is a brother of (\w+)',
    r'(\w+) is the father of (\w+)',
    r'(\w+) is a grandfather of (\w+)',
    r'(\w+) is a son of (\w+)',
    r'(\w+) is a aunt of (\w+)',
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
    match = []
    index = 0
    for i in statement_patterns:
        match = re.match(i, sentence)
        if match == None:
            continue
        else:
            index = statement_patterns.index(i)
            break
    
    if index == 2:
        mother = match.group(1)
        child = match.group(2)
        query = f"mother({mother},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"mother({child}, {mother}); male({mother})"
            if list(prolog.query(new_query)):
                print("That's not possible")
            else:
                prolog.assertz(f"female({mother})")
                prolog.assertz(f"parent({mother})")
                prolog.assertz(f"child({child})")
                prolog.assertz(f"mother({mother},{child})")
                print("Ok, I'll remember that")    
    
    if index == 8:
        father = match.group(1)
        child = match.group(2)
        query = f"father({father},{child})" 
        if list(prolog.query(query)):
            print("I already know that")
        else:
            new_query = f"father({child}, {father}); female({father})"
            if list(prolog.query(new_query)):
                print("That's not possible")
            else:
                prolog.assertz(f"male({father})")
                prolog.assertz(f"parent({father})")
                prolog.assertz(f"child({child})")
                prolog.assertz(f"father({father},{child})")
                print("Ok, I'll remember that")
    
    

def question(sentence):
    if re.match(r'is (\w+) the father of (\w+)', sentence):
        match = re.match(r'is (\w+) the father of (\w+)', sentence)
        father = match.group(1)
        child = match.group(2)
        query = f"father({father}, {child})"
        exist = bool(list(prolog.query(query)))
        if exist:
            print(f"Yes, the father of {child.capitalize()} is {father.capitalize()}")
        else:
            print(f"No, they are not")
    return 0

if __name__ == "__main__":
    main()