# YAL - Yet Another Language

YAL is yet another scripting language(but worse).

# Syntax

### Basic syntax
```rust
fun main() {
    print("Hello, World!");
}
```

### Fibonacci
```rust
fun fib(n) {
	if (n <= 1) {
		ret n;
	}

	ret fib(n - 1) + fib(n - 2);
}

fun main() {
	mut n = 10;
	loop {
		if (n < 0) {
			break;
		}

		println(fib(n));
		n -= 1;
	}
}
```

### Comments
```rust
// This is a single line comment
```

### Variables
```rust
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
```rust
fun main() {
    const a = int(input());
    const b = int(input());
    
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
```rust
fun main() {
    const n = int(input());
    mut i = 1;
    
    loop {
        if (i > n) {
            break;
        }
        println(i);
        i += 1;
    }
}
```

### Arrays
```rust
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
```rust
fun add(a, b) {
    ret a + b;
}
```

### Data
```rust
data Triangle {
    AB,
    AC,
    BC
}

methods Triangle {
    fun area() {
        const s = (@AB + @AC + @BC) / 2;
        ret pow(s * (s - @AB) * (s - @BC) * (s - @AC), 0.5);
    }
}

fun main() {
    const triangle = Triangle {
        AB: 4,
        AC: 5,
        BC: 6
    };
    println(triangle.area());
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
| `\|`      | Bitwise or                 |
| `^`       | Bitwise xor                |
| `~`       | Bitwise not                |
| `&=`      | Bitwise and by rhs         |
| `\|=`     | Bitwise or by rhs          |
| `^=`      | Bitwise xor by rhs         |
| `>>=`     | Right shift by rhs         |
| `<<=`     | Left shift by rhs          |

# Native Functions

| Function | Description                                            | Prototype                                      |
|----------|--------------------------------------------------------|------------------------------------------------|
| print    | Prints arguments to STDOUT                             | `fn print(..) -> None`                         |
| println  | `print`, but with newline at the end                   | `fn println(..) -> None`                       |
| sleep    | Sleeps for `n`  seconds                                | `fn sleep(n: int/float) -> None`               |
| pop      | Removes last element of given array                    | `fn pop(arr: [any]) -> any`                    |
| input    | Prints prompt(if given) and takes user input as string | `fn input(prompt: string?) -> string`          |
| int      | Converts given argument to `int` type                  | `fn int(n: string/int/float) -> int`           |
| float    | Converts given argument to `float` type                | `fn float(n: string/int/float) -> float`       |
| pow      | Applies nth power on x                                 | `fn pow(x: int/float, n: int/float) -> float`  |

