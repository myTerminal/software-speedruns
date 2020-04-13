# [Rust](https://www.rust-lang.org)

> A language empowering everyone to build reliable and efficient software. - [rust-lang.org](https://www.rust-lang.org)

**PS:** You can always a find more detailed explanation of any concept mentioned here in [this online book](https://doc.rust-lang.org/1.30.0/book/2018-edition).

**PPS:** This document is nowhere close to complete and is expected to receive updates as I make progress on Rust.

## Comments

    // This is a comment

OR

    let count = 5; // This is a comment

## Entry point to a program

    fn main() {
        // A lot of useful code will come here
    }

## Macro to print text to `stdout`

    println!("Just wanted to let you know...");

## Bringing a type into the `prelude`

    // Import 'io' to be used directly
    use std::io;

## Creating variables

    // Create an integer variable with implicit typing
    let limit = 5;

    // Create a mutable integer variable with implicit typing
    let mut count = 3;

    // Create a mutable integer variable with explicit typing
    let mut count: isize = 5;

    // Create a mutable string variable to store an empty string
    let mut user_input = String::new();

    // Create a float type variable with implicit typing
    let ratio = 2.0;

    // Create a float type variable with explicit typing
    let ratio: f32 = 1.5;

    // Create a boolean variable with implicit typing
    let shouldDo = true;

    // Create a boolean variable with explicit typing
    let shouldDo: bool = false;

    // Create a char variable
    let prefix = 'p';

## Shadowing a variable

    // Create a variable to spaces
    let spaces = "     ";

    // Shadow the previous variable, create a new one to hold the length
    let spaces = spaces.len();

## Defining a constant

    const MAX_LIMIT: isize = 300;

## Performing numeric operations

    // Addition
    let sum = a + b;

    // Subtraction
    let diff = a - b;

    // Multiplication
    let prod = a * b;

    // Division
    let quotient = a / b;

    // Modulo division
    let remainder = a % b;

## Strings

### Creating strings

    let first_name = String::from("John");

### Concatenating a literal

    let mut statement = String::from("This is Commander Shepard");

    statement.push_str(", and this is my favorite store on the Citadel!");

### Moving string value between variables

    let my_favorite_color = String::from("Blue");

    let their_favorite_color = my_favorite_color;

    // 'my_favorite_color' cannot be accessed anymore as the value has been moved

### Copying string value instead of moving

    let my_favorite_color = String::from("Blue");

    let their_favorite_color = my_favorite_color.clone();

    // 'my_favorite_color' can still be accessed as the value was copied and not moved

## Slices

### String slices

    let statement = String::from("123456789");

    println!("{}", &statement[2..5]); // => 345
    println!("{}", &statement[2..=5]); // => 3456
    println!("{}", &statement[0..5]); // => 12345
    println!("{}", &statement[..5]); // => 12345
    println!("{}", &statement[2..]); // => 3456789

### Array slices

    let numbers = [1, 2, 3, 4, 5];

    let slice = &numbers[1..3];

## Ownership

### Movement of ownership

    fn main() {
        let statement = String::from("We'll make it"); // 'statement' comes to scope

        print_length(statement); // 'statement' goes out of scope

        // 'statement' does not exist anymore
    }

    fn print_length(s: String) {
        println!("The length is: {}", s.len());
    }

### Getting back ownership of a passed value

    fn main() {
        let statement = String::from("We'll make it"); // 'statement' comes to scope

        let statement = print_length(statement); // 'statement' goes out of scope and comes back as a return value

        // 'statement' can be accessed again
    }

    fn print_length(s: String) -> String {
        println!("The length is: {}", s.len());
        s
    }

### Borrowing ownership through references

    fn main() {
        let statement = String::from("We'll make it"); // 'statement' comes to scope

        print_length(&statement); // A reference of 'statement' is passed

        // 'statement' can be accessed as if nothing happened
    }

    fn print_length(s: &String) {
        println!("The length is: {}", s.len());
    }


### Mutable references

    fn main() {
        let statement = String::from("I'm Commander Shepard"); // 'statement' comes to scope

        complete_the_statement(&mut statement); // A mutable reference of 'statement' gets passed, ownership gets borrowed

        // 'statement' can be used, but with changes
    }

    fn complete_the_statement(s: &mut String) {
        s.push_str(", and this is my favorite store on the Citadel!");
    }

### A mutable reference can be the only reference in a scope

#### Multiple immutable references in a scope

    // This is OK
    let last_name = String::from("Shepard");

    let n1 = &last_name;
    let n2 = &last_name;

#### Multiple mutable references in a scope

    // This is not OK
    let last_name = String::from("Shepard");

    let n1 = &mut last_name;
    let n2 = &mut last_name;

#### Multiple mutable references but in different scopes

    // This is OK
    let last_name = String::from("Shepard");

    {
        let n1 = &mut last_name;
    }

    let n2 = &mut last_name;

#### Immutable references when there's a mutable one in the same scope

    // This is not OK
    let last_name = String::from("Shepard");

    let n1 = &last_name;
    let n2 = &last_name;

    let n3 = &mut last_name;

### A reference cannot be dangling

    // This is no OK
    fn get_reference() -> &String {
        let first_name = String::from("John");
        &s
    }

## Reading user input from `stdin`

    use std::io;

    fn main() {
        ...
        io::stdin().read_line(&mut user_input);
        ...

OR

    std::io::stdin().read_line(&mut user_input);

## Printing values to `stdout`

    println!("The count is {} and the length is {}", count, length);

## Converting a string to a number

    let user_input: isize = user_input.trim().parse()
        .expect("A number is required!");

## Compound types: Array

### Creating an array

    // Create an array of integers with implicit typing
    let numbers = [1, 2, 3, 4, 5];

    // Create an array of integers with explicit typing
    let numbers: [isize; 5] = [1, 2, 3, 4, 5];

### Accessing elements

    let second_number = numbers[1];

## Compound types: Tuple

### Creating a tuple

    // Create a tuple with implicit typing
    let name = ("John", "Shepard");

    // Create a tuple with explicit typing (strings)
    let name: (String, String) = (String::from("John"), String::from("Shepard"));

    // Create a tuple with explicit typing (strings slices)
    let name: (&str, &str) = ("John", "Shepard");

### Accessing elements

    let first_name = name.0;
    let last_name = name.1;

### Degeneration

    // Create an array of integers with implicit typing
    let (first_name, last_name) = name;

## Compound types: Structs

### Defining a struct

    struct Name {
        first_name: String,
        last_name: String,
    }

### Creating an instance

    let his_name = Name {
        first_name: String::from("John"),
        last_name: String::from("Shepard"),
    };

### Accessing elements

    let first_name = his_name.first_name;

### Shorthand syntax

    fn build_name(first_name: String, last_name: String) -> String {
        Name {
            first_name,
            last_name,
        }
    }

is equivalent to

    fn build_name(first_name: String, last_name: String) -> String {
        Name {
            first_name: first_name,
            last_name: last_name,
        }
    }

And this valid too

    struct User {
        email: String,
        age: isize,
        weight: f64,
    }

    let old_user = User {
        email: String::from("his_email@domain.com"),
        age: 32,
        weight: 79.2,
    };

    let new_user = User {
        weight: 81.4,
        ..old_user
    };

## Compound types: Tuple Structs

### Defining a Tuple Struct

    struct Color(i32, i32, i32);

### Creating an instance

    let black = Color(0, 0, 0);
    let red = Color(256, 0, 0);

## Blocks & scopes

    { // 'count' does not exist
        let count = 5; // 'count' is valid from this point onwards
        ...
        // 'count' can be access here
        ...
    } // 'count' is no exists

## Statements vs expressions

    // A block that does not return a value
    {
        println!("Entering block...");
        let count = 0;
        ...
        println!("Exiting block..."); // Statement as it ends with a ';'
    }

    // A block that returns a value
    let result = {
        println!("Entering block...");
        let count = 0;
        ...
        count // Expression as it doesn't end with a ';'
    };

## Functions

    fn get_sum(a: isize, b: isize) -> isize {
        return a + b;
    }

OR

    fn get_sum(a: isize, b: isize) -> isize {
        a + b
    }

## If, Else & Else-If

    // A simple if-else
    if count < 5 {
        println!("Not there yet");
    } else {
        println!("We're there");
    }

    // An If-else-if ladder
    if count < 5 {
        println!("Very less");
    } else if count > 5 {
        println!("A little too much");
    } else {
        println!("Just right");
    }

    // 'if-else' for assignment because any block can return a value when the
    // last line is an expression instead of a statement
    let result = if number >= 0 {
        "Positive"
    } else {
        "Negative"
    }

## Looping a set of statements

    loop {
        println!("This will keep getting printed.");
    }

## Breaking from a loop

    // A simple break from a loop
    loop {
        ...
        break;
    }

    // Returning a value while breaking
    let mut count = 0;

    loop {
        count += 1;
        ...
        break count;
    }

## `while` loops

    let mut count = 0;

    while count < 5 {
        println!("Counting {}", count);
        count += 1;
    }

## Looping through a collection

### The more obvious way

    let numbers = [1, 2, 3, 4, 5];
    let mut index = 0;

    while index < 5 {
        println!({}, numbers[index]);
        index += 1;
    }

### A better way

    let numbers = [1, 2, 3, 4, 5];

    for element in a.iter() {
        println!("{}", element);
    }

## Running a countdown without explicit decrement

    for number in (1...5).rev() {
        println!("{}", number);
    }

## Using `match` to compare numbers

    use std::cmp::Ordering;

    fn main() {
        ...
        match user_input.cmp(&count) {
            Ordering::Less => println!("Less"),
            Ordering::Greater => println!("More"),
            Ordering::Equal => println!("Equal"),
        }
        ...

## Handling a potential failure

    io::stdin.read_line($mut user_input)
        .expect("Failed to read user input");

## Handling a potential failure, correctly using `match`

    let user_input: isize = match user_input.trim().parse() {
        Ok(num) => num,
        Err(error) => {
            panic!("There was an error parsing user_input as a number: {}", error)
        },
    };
