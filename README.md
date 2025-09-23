# Game DSL

## Domain Description
The chosen domain is **Game**.  
The goal of this DSL is to describe simple game operations such as creating players, adding scores, starting levels, and defining quests.  

### Main Entities
- **Player**: a character in the game.
- **Score**: numerical points that can be added to a player.
- **Level**: a stage or area of the game that can be started.
- **Quest**: a mission or task to complete. Quests may be recursive (a sequence of quests can contain other quests).

### Main Operations
- `create player <string>` → Create a new player with a given name.
- `add score <integer>` → Add points to the current player’s score.
- `start level <string>` → Start a specific level by name.
- `define quest <quest>` → Define a quest (task or sequence of tasks).
- `dump examples` → Show example commands of the DSL.

---

## Grammar (BNF)

<command> ::= <create_player> | <add_score> | <start_level> | <define_quest> | <dump_examples>

<create_player> ::= "create" "player" <quoted_string>
<add_score>      ::= "add" "score" <integer>
<start_level>    ::= "start" "level" <quoted_string>
<define_quest>   ::= "define" "quest" <quest>
<dump_examples>  ::= "dump" "examples"

<quest> ::= <task> | <sequence>
<task> ::= "task" <quoted_string>
<sequence> ::= "sequence" <quest> <quest>

<quoted_string> ::= "\"" <char>* "\""
<char> ::= [a-z] | [A-Z] | [0-9] | " "
<integer> ::= <digit>+
<digit> ::= [0-9]


