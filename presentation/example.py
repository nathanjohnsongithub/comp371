# Recursive generator to mimic Haskell's lazy evaluation
def fibonacci():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

# Get the first n Fibonacci numbers
def take_fibonacci(n):
    fib_gen = fibonacci()
    return [next(fib_gen) for _ in range(n)]

# Sample main method
if __name__ == "__main__":
    n = int(input("Enter the number of Fibonacci numbers to display:\n"))
    print(take_fibonacci(n))    