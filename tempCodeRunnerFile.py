    person = match.group(1)

        query = f"sister(Sister, {person})"
        sisters = list(prolog.query(query))

        if sisters:
            sister_names = [s["Sister"].capitalize() for s in sisters]
            print(f"The sisters of {person.capitalize()} are: {', '.join(sister_names)}")
        else:
            print(f"{person.capitalize()} has no known sisters.")