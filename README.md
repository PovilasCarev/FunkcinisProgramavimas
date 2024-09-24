# VINYL COLLECTION DOMAIN

## Main Entities

- `<vinyl>` - Represents a vinyl record, including its title, artist, genre, and release year.
- `<vinyl_list>` - Refers to a list of vinyl records that are part of a collection or a selection for operations.
- `<collection>` - Represents the user's vinyl record collection, which can be organized by artist, genre, or year.

## Main Operations

- **Add** a vinyl record to the collection.
- **Remove** a vinyl record from the collection.
- **Update** the details of a vinyl record.
- **View** the vinyl collection or specific records based on criteria.

## Command Examples

- `Add Pink_Floyd_Dark_Side_of_the_Moon_1973_Rock to collection`
- `Remove Nirvana_Nevermind_1991_Grunge from collection`
- `Update Beatles_Abbey_Road_1969_Rock with genre Pop`
- `View all records in Rock genre`

## BNF Grammar for a Vinyl Collection System

```bnf
<command> ::= <action> <vinyl> | <view_command> <criteria>
<action> ::= "Add" | "Remove" | "Update"
<vinyl> ::= <artist> "_" <title> "_" <year> "_" <genre>
<artist> ::= <name>
<title> ::= <name>
<year> ::= <number>
<genre> ::= "Rock" | "Pop" | "Jazz" | "Blues" | "Classical" | "Hip-Hop" | "Electronic" | "Folk" | "Grunge"
<name> ::= <char> {<char>}
<char> ::= [A-Za-z]
<number> ::= <digit> <digit> <digit> <digit>
<digit> ::= [0-9]
<view_command> ::= "View"
<criteria> ::= "all records" | "in" <genre> | "by" <artist> | "released in" <year>
