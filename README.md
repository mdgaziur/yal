# YAL - Yet Another Language

YAL is yet another scripting language(but worse).

# TODO

- Implement interpreter code for interpreting `method` statement

# Gotchas

- When calling a function, it'll inherit the scope of parent function
- Sometimes the interpreter will cause deadlock

# Syntax

### Basic syntax
```go
fun main() {
    print("Hello, World!");
}
```

### Comments
```go
// This is a single line comment
```

### Variables
```go
fun main() {
    // A variable is either const,
    const a = 1;
    // or mutable
    mut b = 2;
    const result = a + b;
    print(result); // 3
}
```

### Conditions
```go
fun main() {
    const a = input();
    const b = input();
    
    if (a > b) {
        print("a is greater than b");
    } else if (a < b) {
        print("a is less than b");
    } else {
        print("a is equal to b");
    }
}
```

### Loops
```go
fun main() {
    const n = input();
    mut i = 0;
    
    loop {
        if (i > n) {
            break;
        }
        print(i);
        i += 1;
    }
}
```

### Arrays
```go
fun main() {
    const arr = [1, 2, 3, 4, 5];
    print(arr[0]); // 1
    print(arr[1]); // 2
    print(arr[2]); // 3
    print(arr[3]); // 4
    print(arr[4]); // 5
    
    // iterate
    iter arr: it {
        print(it);
    }
}
```

### Functions
```go
fun add(a, b) {
    ret a + b;
}
```

### Data
```go
data Triangle {
    AB,
    AC,
    BC,
}

methods Triangle {
    fun area() {
        ret @AB * @AC / 2;
    }
}

fun main() {
    const triangle = Triangle {
        AB: 1,
        AC: 2,
        BC: 3,
    };
    print(triangle.AB); // 1
}
```

### Operators

| Operator  | Description                |
|-----------|----------------------------|
| `+`       | Addition                   |
| `-`       | Subtraction                |
| `*`       | Multiplication             |
| `/`       | Division                   |
| `%`       | Modulo                     |
| `=`       | Assignment                 |
| `==`      | Equal                      |
| `!=`      | Not equal                  |
| `>`       | Greater than               |
| `<`       | Less than                  |
| `>=`      | Greater than or equal to   |
| `<=`      | Less than or equal to      |
| `&&`      | Logical and                |
| `\\`      | Logical or                 |
| `!`       | Logical not                |
| `+=`      | Increment by rhs           |
| `-=`      | Decrement by rhs           |
| `*=`      | Multiply by rhs            |
| `/=`      | Divide by rhs              |
| `%=`      | Modulo by rhs              |
| `>>`      | Right shift                |
| `<<`      | Left shift                 |
| `&`       | Bitwise and                |
| `&#124;`  | Bitwise or                 |
| `^`       | Bitwise xor                |
| `~`       | Bitwise not                |
| `&=`      | Bitwise and by rhs         |
| `&#124;=` | Bitwise or by rhs          |
| `^=`      | Bitwise xor by rhs         |
| `>>=`     | Right shift by rhs         |
| `<<=`     | Left shift by rhs          |
