
# 1.
* Exit removal
<Programme> ::= "Add" <Storage> <Item> | "Delete" <Storage> <Item> | "Restock" <Storage> <Item> | "Sell" <Item> | "Exit"

*Removed "Exit" command: couldn't make the exit for the command nor to reset every state to 0*


# 2.
* <Art store> changed 
<Art store> ::= <Storage> <Writing utensils> <Books> <Art supplies> -> <Art store> ::= <Programme>

*"Art store" just goes to function in which everything starts instead of having additional function that repeat*

# 3. 
* Added quantity
<Programme> ::= "Add" <Storage> <Quantity> | "Delete" <Storage> | "Restock" <Storage> <Quantity> | "Sell" <Storage> <Quantity> | Check <Storage>
*Changed <Item> to <Quantity> since everything will be added to storage, thus we are able to specify the excact numebr to add *

# 4.
* Removed Writing utensils from <Storage>
-> <Storage> ::= <Item> | <Storage> <Item>
*Changed <Writing Utensils> to <Item> since "Writing utensils" are already specified in Items*

# Save command. saving the state
'''
>>> :paste         
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN           
| Add orange 11;  
| Delete fiction; 
| Restock oil 11; 
| Restock oil 11; 
| Sell orange 1;  
| Sell oil 1;     
| Delete graphite;
| END  
|
>>> Save
State saved successfully.
>>>
'''
* Saved state is saved in the file state.txt:
'''
BEGIN
Add orange 10;
Restock oil 21;
Delete graphite;
Delete fiction;
END
'''