# **Kc Language Specification**

## **1. Overview**

Kc is a statically typed, strongly typed language. It emphasizes immutability, type safety, and reference management simplification, aiming for clarity and robustness in data manipulation and control flow.

---

## **2. Types**

### **2.1 Primitive Types**

- `int`: Represents integer values. Passed by value.
- `float`: Represents floating-point numbers. Passed by value.
- `bool`: Represents boolean values (`true` or `false`). Passed by value.
- `string`: Represents a sequence of characters. Immutable and passed by reference.
- `array[T]`: A collection of elements of type `T`. Passed by reference.
- `struct`: A user-defined collection of named fields. Passed by reference.

---

## **3. Memory Management**

- **Pass-by-Reference**: All arrays and structs are passed by reference, allowing efficient handling of larger data structures.
- **Pass-by-Value**: Primitives such as `int`, `float`, and `bool` are passed by value, ensuring no side-effects.
- **No Explicit Reference Management**: There is no need for explicit reference management (no manual `new` or `delete`). The language takes care of reference management implicitly.

---

## **4. Function Overloading**

- **Function Overloading**: Functions in Kc can be overloaded, meaning multiple functions can share the same name as long as they differ in their parameter lists.
- **Resolution**: Function calls are resolved based on the number and types of parameters provided at call time. There is no function overloading solely based on return type.

---

## **5. Operator Overloading**

- **Operator Overloading (Future Addition)**: While operator overloading is not currently supported, the language will allow users to define custom behavior for built-in operators in the future, following the same rules as function overloading.

---

## **6. Strings**

- **Immutability**: Strings are immutable. Once a string is created, it cannot be changed. New strings must be created to reflect modifications.
- **Representation**: Strings are enclosed in double quotes (`"example"`).

---

## **7. Printing**

- **`print` Function**: The built-in `print` function can print any primitive type (`int`, `float`, `bool`, `string`). Custom user types (such as structs) must implement the `str` method to define their string representation.

- **`str` Method for Custom Types**:
  
  ```kc
  func str(x: Type): string
  ```
  
  This method must accept a parameter of the custom type and return a string representing the instance.

---

## **8. Conversion**

- **No Implicit Conversion**: Implicit type conversion is not allowed between types. Every operation must use explicitly compatible types.

- **Explicit Conversion**: The `toType(x: primitive): Result` function can be used for type conversions between primitive types.
  
  - The function will attempt to convert `x` to `Type` and return a `Result` structure indicating success or failure:
    
    ```kc
    struct Result {
      result: Type,
      error: string
    }
    ```
    
    
  
  - If the conversion is successful, `result` contains the value and `error` is an empty string. If it fails, `result` is undefined, and `error` contains an error message.

---

## **9. Initialization**

- **No Null Values**: Kc does not allow uninitialized variables or null values. Every variable must be initialized when it is declared.
  - Future sugar features, such as an option type or sum types, may be introduced to allow for more flexible initialization patterns.

---

## **10. Variable Shadowing**

- **Shadowing Allowed**: Kc allows variable shadowing within nested scopes. A variable declared in an inner scope can have the same name as a variable in an outer scope. The inner scope variable will temporarily "shadow" the outer scope variable, and when the inner scope exits, the outer variable is accessible again.

### **Example**

```kc
var x: int = 10

func example() {
    var x: int = 5  # shadows the outer x
    print(x)        # prints 5
}

print(x)            # prints 10 (outer x)
```



---

## **11. Example Syntax**

### **Function Overloading Example**

```kc
func add(x: int, y: int): int {
    return x + y
}

func add(x: float, y: float): float {
    return x + y
}
```



### **`str` Method for Custom Type**

```kc
struct Person {
    name: string,
    age: int
}

func str(p: Person): string {
    return "Name: " + p.name + ", Age: " + str(p.age)
}
```



### **Explicit Type Conversion**

```kc
var x: int = 10
var y: Result = toFloat(x)

if y.error == "" {
    print("Converted value: ", y.result)
} else {
    print("Conversion failed: ", y.error)
}
```


