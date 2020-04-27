# [Rust](https://www.rust-lang.org)

> A language empowering everyone to build reliable and efficient software. - [rust-lang.org](https://www.rust-lang.org)

**PS:** This document is just a quick-reference for Rust and is not (supposed) to be perfect. You can always find a more detailed explanation of any concept mentioned here in [this online book](https://doc.rust-lang.org/1.30.0/book/2018-edition).

**PPS:** This document is also not complete and most probably not accurate. Please excuse me if I mis-represented any concept of Rust and feel free to contribute with your improvements.

## Comments

    // This is a comment

OR

    let count = 5; // This is a comment

## Entry point to an executable program

    fn main() {
        // A lot of useful code will come here
    }

## Input and output from `std`

### Printing to `stdout`

    println!("Just wanted to let you know..."); // Text

    println!("The values are {} and {}", a, b); // Simple values

    println!("The value is {:?}", value); // Compound values

    println!("{0}+{1}={2} and {0}-{1}={3}", 3, 2, 5, 1); // Values with positions

### Reading  from `stdin`

    use std::io;

    fn main() {
        ...
        io::stdin().read_line(&mut user_input);
        ...

OR

    std::io::stdin().read_line(&mut user_input);

## Bringing a type into the `prelude`

    use std::io; // Import 'io' to be used directly instead of 'std::io'

## Variables

### Creating variables

#### Implicit types

    let limit = 5; // An integer

    let mut count = 3; // A mutable integer

    let mut user_input = String::new(); // A mutable string

    let ratio = 2.0; // A floating point number

    let should_do = true; // A boolean

    let prefix = 'p'; // A character

#### Explicit types

    let mut count: i16 = 5; // A unsigned integer

    let mut count: isize = 5; // A unsigned integer depending on processor bit-length

    let mut level: u16 = -3; // A signed integer

    let mut level: usize = -3; // A signed integer depending on processor bit-length

    let ratio: f64 = 1.5; // A floating point number

    let should_do: bool = false; // A boolean

    let prefix: char = 'p'; // A character

## Constants

    const MAX_LIMIT: isize = 300; // Global constant without an address (like pre-processor directives in 'C')

    statis MAX_LIMIT: isize = 300; // Global constant that can be used by reference

## Blocks & Scopes

### Basic scoping

    { // 'count' does not exist
        let count = 5; // 'count' is valid from this point onwards
        ...
        // 'count' can be access here
        ...
    } // 'count' is no exists

### Shadowing inside an inner scope

    let count = 0; // 'count' defined and exists

    {
        let count = 5; // A different 'count' is defined
        ...
        // Inner 'count' can be access here
        ...
    } // Outer 'count' takes over

### Shadowing within the same scope

    let spaces = "     "; // Create a variable to store spaces

    let spaces = spaces.len(); // Shadow the previous variable, create a new one to hold the length

## Statements vs expressions

### A block that does not return a value

    {
        println!("Entering block...");
        let count = 0;
        ...
        println!("Exiting block..."); // Statement as it ends with a ';'
    }

### A block that returns a value

    let result = {
        println!("Entering block...");
        let count = 0;
        ...
        count // Expression as it doesn't end with a ';'
    };

## Operators

### Simple numeric

    let sum = a + b; // Addition

    let diff = a - b; // Subtraction

    let prod = a * b; // Multiplication

    let quotient = a / b; // Division

    let remainder = a % b; // Modulo division

### More numeric

    let cubed = i32::pow(a, 3); // Power

    let cubed = f64::powi(b, 3); // Integral power

    let powered = f64::powf(b, 2.5); // Floating-point powers

    let square_root = 2f64.sqrt() // Square root

### Comparison

    let is_greater = a > b; // Greater-than

    let is_greater_or_equal = a >= b; // Greater-than or equal

    let is_lesser = a < b; // Lesser-than

    let is_lesser_or_equal = a <= b; // Lesser-than or equal

    let is_equal = a == b ; // Equal

    let is_not_equal = a != b ; // Not equal

### Bitwise

    let b = !a; // NOT

    let c = a | b; // OR;

    let c = a & b; // AND;

    let c = a ^ b; // XOR;

    let one_zero_two_four = 2 << 10; // Left-shift

    let two = one_zero_two_four >> 10; // Right-shift

## Stack and Heap

    let x = 5; // Implicitly stored on stack

    let x = Box::new(5); // Explicitly made to be stored on heap
    let value_of_x = *x; // Dereference the variable to get its value back on stack

## Conditionals

### A simple if-else

    if count < 5 {
        println!("Not there yet");
    } else {
        println!("We're there");
    }

### An If-else-if ladder

    if count < 5 {
        println!("Very less");
    } else if count > 5 {
        println!("A little too much");
    } else {
        println!("Just right");
    }

### Assignment through 'if-else'

    let result = if number >= 0 {
        "Positive"
    } else {
        "Negative"
    }

## Looping

### Simple `loop`

#### Looping a set of statements indefinitely

    loop {
        println!("This will keep getting printed.");
    }

#### Skipping loop executions

    let mut count = 0;

    loop {
        if count == 2 {
            continue; // Skip this cycle
        }
        ..
        count += 1;
    }

#### Breaking from a loop

    loop {
        ...
        break; // Just quit the loop
    }

#### Breaking and returning a value

    let mut count = 0;

    let result = loop {
        count += 1;
        ...
        break count; // Quit and return the 'count'
    }

### `while` loops

#### A simple `while` loop

    let mut count = 0;

    while count < 5 {
        println!("Counting {}", count);
        count += 1;
    }

#### Looping through a collection

##### The conventional way with `while`

    let numbers = [1, 2, 3, 4, 5];
    let mut index = 0;

    while index < 5 {
        println!({}, numbers[index]);
        index += 1;
    }

##### A better way with `for`

    let numbers = [1, 2, 3, 4, 5];

    for element in a.iter() {
        println!("{}", element);
    }

### `for` loops

#### Looping through a simple range

    for x in 1..11 { // Runs 1 through 10
        println!("{}", x);
    }

#### Looping along with indices and values

    for (i, x) in (1..11).enumerate() {
        println!("{}: {}", i, x);
    }

#### Running a countdown without explicit decrement

    for number in (1...5).rev() {
        println!("{}", number);
    }

## `Match`ing instead of `switch`ing

### A simple lookup using `match`

    let code = 91;

    let country = match code {
        1 => "United States",
        91 => "India",
        _ => "not yet known" // `_` works for all remaining values
    };

### Lookup with ranges

    let difference = 0;

    let status = match difference {
        1..=1000 => "More", // Between 1 and 1000
        -1000..=-1 => "Less", // Between -1 and -1000
        0 => "Equal", // Zero
        _ => "Unknown" // All remaining values
    };

### A simple pattern matching

    let count = 6;

    let count_in_words = match count {
        0 => "None",
        1 => "One",
        2 | 3 => "A couple of", // 2 or 3
        4..=5 => "A handful",
        _ if (count % 2 == 0) => "Some even number more than 5",
        _ => "Too many" // Else
    };

### A relatively complex pattern matching

    enum Attribute {
        CanFly, // Symbol
        Weight(u16), // Tuple
        Height(u8,u8), // Tuple,
        Locations{homes: u8, offices: u8} // Struct
    };

    let attribute = Attribute::Weight(210);

    match attribute {
        Attribute::CanFly => println!("So you can fly, huh?"), // A simple test for a possible option
        Attribute::Weight(200..=u16::MAX) => println!("You weigh just too much!"), // Test for a range within a Tuple's element
        Attribute::Weight(w) => println!("You weigh {} lbs", w), // Test for a possible option with a value handler
        Attribute::Height(7, _) => println!("That's a little too tall!"), // Test for a particular characteristic in one of the Tuple's elements
        Attribute::Height(f, i) => println!("So you're {}' {}\"", f, i), // A generic test for existence of a particular possible option
        Attribute::Locations{homes:2..=u8::MAX,..} => println!("Oh, you must be very rich!"), // Using '..' for values that don't need to be specified or used
        Attribute::Locations{homes: _, offices: 0} => println!("So you're working from home these days?"), // Test for a pattern within a struct
        Attribute::Locations{homes: h, offices: o} => println!("You have {} homes and {} offices", h, o), // A regular pattern match
    }

### Using statements inside `match`

    let difference = 0;

    match difference {
        1..=1000 => println!("More"), // Between 1 and 1000
        -1000..=-1 => println!("Less"), // Between -1 and -1000
        0 => println!("Equal"), // Zero
        _ => println!("Unknown") // All remaining values
    };

### Comparing numbers

    use std::cmp::Ordering;

    fn main() {
        ...
        match user_input.cmp(&count) {
            Ordering::Less => println!("Less"),
            Ordering::Greater => println!("More"),
            Ordering::Equal => println!("Equal"),
        };
        ...

## Functions

### A function with no return value

    fn print_count(count: usize) {
        println!("Count: {}", count);
    }

### A function with a return value

    fn get_sum(a: isize, b: isize) -> isize {
        return a + b; // Explicit return
    }

OR

    fn get_sum(a: isize, b: isize) -> isize {
        a + b // Expression acts as a return value
    }

### Passing values directly

    fn print_count(count: usize) {
        println!("Count: {}", count);
    }

    let count = 3;

    print_count(count);

### Passing values by reference

    fn print_count(count: &usize) {
        println!("Count: {}", &count);
    }

    let count = 3;

    print_count(&count);

### Using mutable references to change the argument's value

    fn increment_and_print_count(count: &mut usize) {
        *count += 1;
        println!("Count after increment: {}", &count);
    }

    let mut count = 3;

    println!("Original count: {}", count);

    increment_and_print_count(&mut count);

### Storing a function as a variable

    let double = |x| x * 2;

    println!("Double of {} is {}", 4, double(4));

### Creating a closure by referencing a variable from outside

    let incr = 2;

    let add = |x| x + incr;

    println!("Increment of {} is {}", 4, add(4));

## Higher-order functions

    let result = (0..100) // Range of numbers from 0 to 99
        .filter(|i| i % 2 == 0) // A lazy filter for even numbers
        .map(|n| n * n * n) // A lazy map to cube each number
        .fold(0, |sum, n| sum + n); // Finally gets summed into a result

> TODO: Add more details

## Compound Data Types

### Arrays

#### Creating an array

    let numbers = [1, 2, 3, 4, 5]; // An array with implicit typing

    let numbers: [isize; 5] = [1, 2, 3, 4, 5]; // An array with explicit typing

#### Accessing elements

    let second_number = numbers[1];

#### Modifying elements

    let mut numbers = [1, 2, 3, 4, 5]; // A mutable array

    numbers[0] = 99; // Modify an element as you would a variable

#### Iterating on elements

##### Using `len()`

    for i in 0..numbers.len() {
        println!("{}", numbers[i]);
    }

##### Using `iter()`

    for x in numbers.iter() {
        println!("{}", x);
    }

#### Checking for equality

    if numbers != [1, 2, 3, 4, 5] {
        println!("The array is not the same anymore");
    }

#### Filling an array

    let ten_ones = [1; 10];

#### Creating multi-dimensional arrays

    let numbers: [[isize; 3]; 2] = [
        [1, 2, 3],
        [7, 8, 9]
    ];

#### Accessing elements from a multi-dimensional array

    let result = numbers[1][2];

### Vectors

#### Creating a vector

    let mut numbers = Vec::new();

#### Adding values to a mutable vector

    numbers.push(1);
    numbers.push(2);

#### Accessing elements

    let first_number = numbers[0];

#### Safely accessing elements

    let some_number = numbers.get(7);

    match some_number {
        Some(n) => println!("some_number={}", n),
        None => println!("There isn't such an element"),
    };

#### Iterating on elements

    for x in &numbers {
        println!("{}", x);
    }

#### Removing values

    let last_number = numbers.pop();

    match last_number {
        Some(n) => println!("last_number={}", n),
        None => println!("numbers is empty!")
    }

### Strings

#### Creating strings

    let first_name = String::from("John");

#### Appending a string

    let mut statement = String::from("This is Commander Shepard");

    statement.push_str(", and this is my favorite store on the Citadel!");

#### Concatenating a character

    let mut statement = String::from("This is Commander Shepard");

    statement.push('!');

#### Removing a character

    statement.remove(0); // Remove first character

#### Replacing parts of a string

    let statement = String::from("I'm Commander Shepard");

    let new_statement = statement.replace("I'm ", "I'm not ");

#### Moving string value between variables

    let my_favorite_color = String::from("Blue");

    let their_favorite_color = my_favorite_color;

    // 'my_favorite_color' cannot be accessed anymore as the value has been moved

#### Copying string value instead of moving

    let my_favorite_color = String::from("Blue");

    let their_favorite_color = my_favorite_color.clone();

    // 'my_favorite_color' can still be accessed as the value was copied and not moved

#### Converting into a string slice

    let string_slice = &my_favorite_color;

#### Concatenation

    let my_favorite_color = String::from("Blue");

    let string_slice = ", of course!";

    let x = my_favorite_color + string_slice; // This will work (String + &str)

    let y = my_favorite_color + String::from(", of course"); // This will not work (String + String)


### Slices

#### String slices

##### Creating a slice

    let statement = "I'm Commander Shepard";

OR

    let statement:&'static str = "I'm Commander Shepard";

##### Getting characters from a string slice

    for x in statement.chars() {
        println!("{}", x);
    }

    for x in statement.chars().rev() { // In reverse
        println!("{}", x);
    }

##### Safely getting a character from a string slice

    if let Some(x) = statement.chars().nth(4) {
        println!("{}", x);
    } else {
        println!("The statement is empty!");
    }

##### Converting into a string

    let statement_as_string = statement.to_string();

##### Extracting a slice from a string

    let statement = String::from("123456789");

    println!("{}", &statement[2..5]); // => 345
    println!("{}", &statement[2..=5]); // => 3456
    println!("{}", &statement[0..5]); // => 12345
    println!("{}", &statement[..5]); // => 12345
    println!("{}", &statement[2..]); // => 3456789

#### Array slices

    let numbers = [1, 2, 3, 4, 5];

    let slice = &numbers[1..3]; // A slice of the array 'numbers'

    let length_of_slice = slice.len(); // Length of an array slice

### Tuples

#### Creating a tuple

    let name = ("John", "Shepard"); // Implicit typing

    let name: (&str, &str) = ("John", "Shepard"); // Explicit typing

#### Accessing elements

##### Using index

    let first_name = name.0;
    let last_name = name.1;

##### Using deconstruction

    let (first_name, last_name) = name;

#### Returning from functions

    fn sum_and_difference(a: isize, b: isize) -> (isize, isize) {
        (a + b, a - b)
    }

### Structs

#### Defining a struct

    struct Name {
        first_name: String,
        last_name: String,
    }

#### Creating an instance

    let his_name = Name {
        first_name: String::from("John"),
        last_name: String::from("Shepard"),
    };

#### Accessing elements

    let first_name = his_name.first_name;

#### Shorthand syntax - 1

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

#### Shorthand syntax - 2

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

#### Defining methods for a struct

    struct User {
        first_name: String,
        last_name: String
    }

    impl User {
        fn full_name(&self) -> String {
            String::from(&self.first_name) + " " + &self.last_name
        }
    }

    let user = User{first_name: "John".to_string(), last_name: "Shepard".to_string()};

    println!("{}", user.full_name());

### Tuple Structs

#### Defining a Tuple Struct

    struct Color(i32, i32, i32);

#### Creating an instance

    let black = Color(0, 0, 0);
    let red = Color(256, 0, 0);

### Enumerations

#### Defining an enum

    enum Color {
        Red,
        Green,
        Blue
    }

#### Creating and instance

    let color: Color = Color::Red;

#### `match`ing to consume enums

    match color {
        Color::Red => println!("It's red!"),
        _ => println!("It's some other color.")
    };

### Options

#### Some and None

    let dividend = 5.0;
    let divisor = 2.0;

    let quotient: Option<f64> = if divisor == 0.0 {
        None
    } else {
        Some(dividend / divisor)
    };

    match quotient {
        Some(n) => println!("{}/{}={}", dividend, divisor, n),
        None => println!("A division is not possible"),
    };

#### `if let`

    let dividend = 5.0;
    let divisor = 2.0;

    let quotient: Option<f64> = if divisor == 0.0 {
        None
    } else {
        Some(dividend / divisor)
    };

    if let Some(n) = quotient {
        println!("{}/{}={}", dividend, divisor, n);
    } else {
        println!("A division is not possible");
    }

#### `while let`

    let mut numbers = Vec::new();

    numbers.push(1);
    numbers.push(2);
    numbers.push(3);

    while let Some(x) = numbers.pop() {
        println!("last_number={}", x)
    }

### Generics

#### Defining a generic type

    struct Coordinate<T> {
        x: T,
        y: T
    }

#### Creating an instance

    let coordinate1 = Coordinate{x: 1.2, y: 3.4}; // Implicit

    let coordinate1<f64> = Coordinate{x: 1.2, y: 3.4}; // Explicit

    let coordinate2 = Coordinate{x: 1, y: 3.4}; // Both values are NOT the same type and will not work

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

## Type-casting and type-conversions

### Type-casting between primitive types

    let n = 'a' as u8; // Convert the character 'a' into an unsigned integer

    let a = 97 as char; // Convert number 97 into a character

> TODO: Add more examples

### Converting a string to a number

    let user_input: isize = user_input.trim().parse()
        .expect("A number is required!");

## Traits

### Defining a trait

    trait Vehicle {
        fn start(&self) { // Declared and defined
            println!("Starting...");
        }

        fn run(&self); // Only declared
    }

### Implementing a trait

    struct SpaceVehicle {
        name: &'static str,
        speed: usize
    }

    impl Vehicle for SpaceVehicle {
        fn run(&self) {
            println!("Flying at speed: {}", &self.speed);
        }
    }

    let sr2 = SpaceVehicle{name: "Normandy", speed: 1000};

    sr2.start(); // Prints "Starting..."
    sr2.run(); // Prints "Flying at speed: 1000"


### Overriding trait functions

    impl Vehicle for SpaceVehicle {
        fn start(&self) {
            println!("Please wait..."); // This 'start' function overrides the default one
        }

        fn run(&self) {
            println!("Flying at speed: {}", &self.speed);
        }
    }

    let sr2 = SpaceVehicle{name: "Normandy", speed: 1000};

    sr2.start(); // Prints "Please wait..."
    sr2.run(); // Prints "Flying at speed: 1000"

### Declaring static functions in traits

    trait Vehicle {
        fn create(name: &'static str, speed: usize) -> Self // Imposes a static function for all implmenting types

        fn start(&self) {
            println!("Starting...");
        }

        fn run(&self);
    }

    impl Vehicle for SpaceVehicle {
        fn create(name: &'static str, speed: usize) -> SpaceVehicle { // A SpaceVehicle returns a SpaceVehicle from 'create'
            SpaceVehicle{name, speed}
        }

        fn start(&self) {
            println!("Please wait..."); // This 'start' function overrides the default one
        }

        fn run(&self) {
            println!("Flying at speed: {}", &self.speed);
        }
    }

    let sr2 = SpaceVehicle::create("Normandy", 1000);

## Handling potential failures

### 'Cheaper' way

    io::stdin.read_line($mut user_input)
        .expect("Failed to read user input");

### Comprehensive way using `match`

    let user_input: isize = match user_input.trim().parse() {
        Ok(num) => num,
        Err(error) => {
            panic!("There was an error parsing user_input as a number: {}", error)
        },
    };

## Unsafe code

> TODO: Add section

## Working with crates

### Using an external crate

1. Reference the crate in *Cargo.toml* file
2. Use `extern cate rand;` on the first line of a file to declare that you'll be using external create called 'rand'
3. Bring relevant symbols to prelude with `use rand::Rng;`
4. Consume the features from the imported external crate

### Creating your own crate

- Create a *Cargo.toml* file to define crate metadata
- Create a *main.rs* for an executable create or a *lib.rs* for a library crate
- Library crates have code organized in `mod` (modules)
- Mark usable symbols as `pub` (public)

## Unit testing

### A sample regular test

    #[test]
    fn does_the_job() {
        assert_eq!(1, module::submodule::get_value())
    }

### A sample test expected to fail

    #[test]
    #[should_panic]
    fn fails_when_required() {
        assert_eq!(2, module::submodule::get_value())
    }

### A sample ignored test

    #[test]
    #[ignore]
    fn does_not_work() {
        assert_eq!(-1, module::submodule::get_value())
    }

## Docs

### Sample module headers

    //! This is an awesome module
    //! It works very good
    //!
    //! # Examples
    //! ```
    //! let result = awesome_function(1, 2);
    //! ```

### Sample function headers

    /// The function below is awesome and always returns a `true`
    fn awesome_function() {
        true
    }

### Command to generate docs

    rustdoc <module_name.rs>
