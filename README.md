## GIMP

GIMP is an IMP-like simple imperative programming language 

### Primary Data Type:

The primary data types we are going to use in GIMP are Integer and Boolean.

1. Integer (Int): all integer values from negative to positive including zero
2. Boolean (Bool): all boolean values which are true and false

When the variables are declared, the user can choose to immediately assign them a value or to assign a value later on.

### Data Structure:

The data structure we are going to use in GIMP is an array.

1. Array: holds elements of primary data type, for now, it’s implemented to hold only Int values.

### Control Structure:

Here are the fundamental Control Structures in GIMP

1. If (Control) Structure
2. IF ELSE: The else statement is optional and will execute only if the condition in the if statement evaluates to false.
3. While (Repetition) Structure
4. WHILE DO: execute a block of statements continuously until the given condition evaluates to false
5. Assignment and Declaration Operators
6. Assignment: Assigning values into the given variable
7. Declaration: declarations specify the data type of the variable we’re declaring
8. Skip: a skip performs a jump and does nothing

### Operators:

Helps us to perform operations on variables, in GIMP we implemented operators based on the type expressions. We can see detailed information in the grammar.

### How to Run GIMP:

All the directives to run the GIMP interpreter are present in Main.hs. Running using `runghc` will let you run the file.

1. Navigate to the project directory
2. Run main.hs using this command: `runghc Main.hs`
3. You will be presented with two options: “Run program” and “Exit”
4. After typing “1” for “Run program”
5. Type your test code in a single line
6. Hit enter and you will get the result
