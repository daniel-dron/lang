## variables

simple variable declaration. the type is automatically infered from its initializer
``` rust
    let foo = 10;
    foo = 20;
```

variable shadowing is possible in both the same scope and nested, having different effects
``` rust
    // in the same scope, the original variable will no longer be accessible
    let foo = "i'm alive!";
    let foo = 1337;

    // original str foo is no longer with us
    print(foo); // > 1337 
```

``` rust
    // in different scopes, the original will come back into scope
    let foo = "i'm immortal (for a while)";
    {
        let foo = 2025;
        print(foo); // > 2025
    }
    print(foo); // > i'm immortal (for a while)
```

## types

while types can be infered, they can still be anotated on variables, function parameters and return types
this is done after the variable name using `: type` or after the function parameters using `-> type`
``` rust
    let foo: f64 = 100000;

    fn bar(arg1: f64, arg2: str) -> str {
        return arg1 + arg2; // concatenation
    }
```

### custom types

```rust

    type Person {
        name: str,
        age: f64
    }

    binds Person {
        fn description(ref self) -> str {
            return self.name " is " + self.age + " years old";
        }
    }

    let person: Person = Person { name: "Michael", age: 23 };
    print(person.description);
```

### array

arrays are dynamic. but can only hold one type
``` rust

    let list: [f64] = [1, 2, 3, 4];
    let val: f64 = list[2]; // returns value or panics
    list[2] = 10; // [1, 2, 10, 4]

    list
```

### copy and ref

for primitive types, the default operation is always copy
``` rust
    let x: f64 = 10;
    let y = x; // 10 is copied
```

for reference types, the default operation is always ref
``` rust
    let x: [f64] = [ 1, 2, 3, 4 ];
    let y = x; // has reference to x

    y[2] = 100;
    print(x); // [1, 2, 100, 4]
```

these are also the default behaviour when passing stuff as parameters
```rust

    fn append_double(vec: [f64], val: f64) {
        vec.append(val * 2);
    }

    let vec: [f64] = [1, 2, 3];
    append_double(vec, 4);

    // vec == [1, 2, 3, 8]
```

you can also choose which method to use to assign or pass parameters
```rust
    fn double(val: ref f64) {
        val = val*2;
    }

    let x = 10;
    double(ref x); // x == 20 now

    fn do_stuff(vec: [f64], f64) -> [f64] {
        vec.append(f64);
    }

    let vec = [1, 2, 3, 4];
    let new_vec = do_stuff(copy vec, 5); // new_vec == [1, 2, 3, 4, 5]
                                         // vec     == [1, 2, 3, 4] 
```

## functions

functions work like in any other language. they take in zero or more arguments and optionally return one
``` rust

    fn super_advanced_sum(a: f64, b: f64) -> f64 {
        return a + b;
    }

    fn returns_nothing(text: str) {
        print("This function returns nothing but prints " + text);
    }
```

function calls are also familiar
``` rust

    let sum = super_advanced_sum(10, 20); // sum: f64 = 30

    returns_nothing("never does anyway..."); // we are not allowed to save this into a variable since () nothing is returned

    let foo = returns_nothing("void as always"); // ERROR
```

## conditionals

conditions are really just if statements
``` rust

    if big_number > smaller_number {
        print("Big numbers on top!");
    } else if big_number == smaller_number {
        print("We are all equal anyway.");
    } else {
        print("Not so big after all...");
    }
```

match patterns are a really usefull idiom for branching based on a single value
they are exhaustive, so you either process all possible values of the expression you are matching on, or you can fallback to `_ => `
``` rust
    match result {
        "success" => print("I knew we could do it"),
        "failure" => print("There's always tomorrow"),
        _ => print("What the hell are we even doing here?")
    };
```

## loops

loops are based on range, and do not allow explicit handling of index like C/C++/Java etc
``` rust
    for i in 0..100 {
        print(i); // 0 1 2 3 4 5, ..., 99
    }
```

ranges are inclusive on the start and exclusive on the end. but you can change that with an `=`
``` rust
    for i in 0..=100 {
        print(i); // 0 1 2 3 4 ... 100
    }
```

you can iterate directly on iterable objects like arrays and strings
``` rust

    for c in "hello world" {
        print(c + ","); // h, e, l, l, o, , w, o, r, l, d,
    }

    let array: [f64] = [0..=100];
    for i in array {
        print(i); // 0 1 2 3 ... 100
    }
```

## closures

## errors and optionals

errors are sumtype of success and error. they can be matched on and passed through the callstack
error values are of type `err<v, e>` where v is the type in case of success and e in case of error

```rust

    let result: err<f64, str> = ok(10);
    let failure: err<f64, str> = fail("job failed boss O7");
```

errors can be matched on
```rust
    match result {
        ok(value) => print("Success " + value + "!"),
        fail(error) => print("Failed... " + error)
    };

    if let value)
```

they can be propagated up the call stack **IF** they have the same fail type with a question mark after the expression `?`
``` rust

    fn bar() -> err<f64, str> {
        return fail("we can never succeed...");
    }

    fn foo() -> err<str, str> {
        let value = bar()?; // the question mark `?` indicates that if bar returns a fail, return that same fail from foo
        return "we got this value: " + value; 
    }
```

optionals are a type that represent the possible absense of value. work similarly to rust
their type are of `opt<v>` where v is the possible value type
they can either be `some(value)` or just `none` in its absense
``` rust
    fn get_prime_between(lower: f64, higher: f64) -> opt<f64> {
        // ...
        return none;
    }

    fn foo() {
        if let some(value) = get_prime_between(54, 58) {
            print("somehow we got a prime! " + value);
        } else {
            print("yhea there is no prime...");
        }

        // or similarly
        if let none get_prime_between(54, 58) {
            print("yhea there was no prime as expected...");
        }
    }
```

## native functions

## std