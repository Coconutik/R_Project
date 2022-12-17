\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{amsmath}
\usepackage{amsfonts}

\title{Prime Numbers}
\author{Marton Zorenyi, Peter Pavicic, Gabor Antal}
\date{November 2021}

\begin{document}
\SweaveOpts{concordance=TRUE}

\maketitle


\section{Functions}

\subsection{primes()}

Let us look at our implementation of the sieve of Eratosthenes, the primes() function.

<<primes, echo=TRUE, eval=TRUE>>=
#install.packages("knitr")
#install.packages(*.sty)
#library('knitr')
#tinytex::parse_install(
#  text = "! LaTeX Error: File `ocgbase.sty' not found."
#)

primes <- function(n, method = "eu") {
  # n: number up to which we sieve
  # method: method for sieving
  
  # CHECKING MISSING ARGUMENTS, SPECIAL CASES
  
  if (missing(n)) {
    stop("Please specify the number of prime numbers to be sieved.")
    
  } else if (n <= 1) {
    
    return(c())
    
    # check if method is not any specified methods
  } else if ( all(!(method == c("er", "er-rec", "eu", "eu-rec"))) ) {
    stop(paste0("Please use any of the following methods: 
        \n'er' for the original sieve of Eratosthenes", 
               "\n'er-rec' for the recursive version of the original sieve", 
               "\n'eu' for Euler's variant", 
               "\n'eu-rec' for a recursive version of Euler's variant"))
  }
  
  
  # FUNCTION DEFINITIONS
  
  er <- function() {
    # setting all numbers as primes with the exception of 1.
    nums <- c(FALSE, rep(TRUE, n - 1))
    p <- 2
      while (p^2 <= n) {
        mark <- c(rep(FALSE, p), (p + 1):n %% p == 0) # marking multiples of p
        nums[mark] <- FALSE # marking multiples of p as not primes
        p <- p + which(nums[(p + 1):n])[1] # selecting first prime after p
      }
    
    which(nums)
  }
  
  er_rec <- function(p = 2, nums = c(FALSE, rep(TRUE, n - 1))) {
    if (p^2 > n) {
      
        which(nums)
    
      } else {
        mark <- c(rep(FALSE, p), (p + 1):n %% p == 0)
        nums[mark] <- FALSE
        p <- p + which(nums[(p + 1):n])[1]
        
        er_rec(p, nums)
    }
  }
  
  primesieve <- function(sieved, unsieved) {
    ## finds primes using the Sieve of Eratosthenes
    ## sieved: sorted vector of sieved numbers
    ## unsieved: sorted vector of unsieved numbers
    p <- unsieved[1]
    n <- unsieved[length(unsieved)]
    if (p^2 > n) {
      return(c(sieved, unsieved))
    } else {
      unsieved <- unsieved[unsieved %% p != 0]
      sieved <- c(sieved, p)
      return(primesieve(sieved, unsieved))
    }
  }
  
  eu_rec <- function() {
    primesieve(c(), 2:n)
  }
  
  eu <- function() {
    sieved <- c()
    unsieved <- 2:n
    p <- 2
    last <- n
    
    while (p^2 <= last) {
      p <- unsieved[1]
      last <- unsieved[length(unsieved)]
      unsieved <- unsieved[unsieved %% p != 0]
      sieved <- c(sieved, p)
    }
    
    c(sieved, unsieved)
  }
  
  
  
  # MAIN PART OF FUNCTION
  
  if (method == "er") {
      er()
  } else if (method == "er-rec") {
      er_rec()
  } else if (method == "eu-rec") {
      eu_rec()
  } else if (method == "eu") {
      eu()
  }
}
@

  \subsubsection{Missing arguments, special cases}

The method takes as arguments the number $n$ until which we sieve for prime numbers and the method, one of "er", "er-rec, "eu", or "eu-rec", which stand for the original sieve of Eratosthenes, the original's recursive implementation, Euler's variant of the sieve, and Euler's variant's recursive implementation respectively.
The default argument method is Euler's variant with `while` loops, which is the fastest of the 4 algorithms.

We first check for special cases which do not require any actual sieving, and continue with the definitions for the sieves for the general case where the function is used correctly.

  \subsubsection{Function definitions}

In the function "er" we use the Sieve of Eratosthenes. We start with a logical vector $nums$, where the first entry is $false$, the following ones are $true$ because 1 is known to not be a prime, and we assume that all the other numbers are primes. We run a while loop, where we mark the multiples of a number $p$ as $false$ and look for the next number that is not a multiple of $p$. This is our new $p$, and we continue until $n$.

What is important to note is that we only have to check until $\sqrt{n}$ to see if $n$ has any factors, 
since $n = x \cdot y = \sqrt{n} \cdot \sqrt{n}$. So if we decrease $x$, we have to increase $y$ in order to obtain $n$. Therefore, if $n$ has any factors $x, y \in \mathbb{Z}, x \leq y$, then $x \leq \sqrt{n} \leq y$. 
Hence, if $n$ has any factors, we would find at least one less or equal to $\sqrt{n}$.

We then had to write two functions using Euler's variant of the sieve. The difference here is that we omit the multiples of $p_1, p_2, ..., p_n$ from the unsieved list for each $p$. 

  \subsubsection{Main part}
  
In the main part of the function we check the given method and call the relevant functions.



\subsection{prime.twins()}

Let us now examine our function for prime twins, prime.twins()

<<primetwins, echo=TRUE, eval=TRUE>>=
prime.twins <- function(n) {
  # n: number up to which we look for prime twins
  # method: method for sieving
  
  # No prime twins until 2-3
  if (n < 3) {
    return(c())
  }
  
  nums <- primes(n, "eu")
  res <- c()
  
  # checks along the vector nums for pairs of twins
  # and adds them as a new row to 'res'
  for (i in 1:(length(nums) - 1)) {
    if (nums[i] + 2 == nums[i + 1]) {
      res <- rbind(res, c(nums[i], nums[i + 1]) )
    }
  }
  
  res
}
@

Here we also distinguish between the special case where there's no prime twins for $n < 3$ and the general case.
In the general case we simply sieve the primes up to $n$ using the fastest method, Euler's non-recursive variant. We then run a for loop which checks every pair of consecutive primes once and saves them if they constitute prime twins.

\section{Computational experiments}

\subsection{Prime numbers}

All prime numbers up to 1000:

<<runprimes, echo=TRUE, eval=TRUE>>=
primes(1000)
@

\subsection{Prime twins}

All prime twins up to 1000:

<<runprimetwins, echo=TRUE, eval=TRUE>>=
prime.twins(1000)
@


\subsection{Runtimes}

<<runtimes, echo=TRUE, eval=TRUE>>=
system.time(primes(1e5, "er"))
system.time(primes(1e6, "er"))
system.time(primes(1e7, "er"))

system.time(primes(1e5, "er-rec"))
system.time(primes(1e6, "er-rec"))
# system.time(primes(1e7, "er-rec"))

system.time(primes(1e5, "eu"))
system.time(primes(1e6, "eu"))
system.time(primes(1e7, "eu"))

system.time(primes(1e5, "eu-rec"))
system.time(primes(1e6, "eu-rec"))
system.time(primes(1e7, "eu-rec"))
@

The larger n got, the longer were the running times.
We observed that the running times of the functions using Euler's variant have much smaller running times, e.g., they computed the number of primes for $n = 10^7$ as fast as the original variant did for $n = 10^6$. A reason for that is that numbers with multiple prime factors are marked multiple times using the original sieve of
Eratosthenes, whereas in Euler's method they are omitted from the unsieved list after being marked for the first time. 
The respective recursive functions were, in general, slower than the ones using a while loop. This could be because of the added time for managing the runtime stack.
That is, because recursive functions call themselves again and again, due to that repetitive function
calling the running time can get higher, whereas in loops a set of instructions is executed repeatedly and more efficiently. It is also preferable to avoid recursive functions as object-oriented programming languages tend to be a bit more optimised for running while and for loops.

It is also worth noting how the recursive version of the original sieve crashes with $n = 10^7$, most probably because an upper cap for the consumption of RAM has been reached.

\subsection{Frequencies of pairs of final digits}

\subsubsection{Function}

Since we check last digit pairs for 4 different number bases, we wrote a function so as to refactor the code.

<<freq_func, echo=TRUE, eval=TRUE>>=
last_digit_pair_distr <- function(prime_vector, modulo = 10) {

  last_digits <- as.integer(prime_vector %% modulo)
  # Creating the vector of remainders mod 'modulo'
  
  # s: determines starting point of looking at the last digit pairs
  # combs: contains the possible combinations of last digit pairs
  if (modulo == 10) {
    combs <- paste(
      c(rep(1, 4), rep(3, 4), rep(7, 4), rep(9, 4)),
      rep(c(1, 3, 7, 9), 4), sep = '-')
    s <- 4 # skips first 3 primes: 2, 3, 5
    
  } else if (modulo == 2) {
    combs <- c("0-0", "0-1", "1-0", "1-1")
    s <- 1 # skips no primes
    
  } else if (modulo == 8) {
    combs <- paste(
        c(rep(1, 4), rep(3, 4), rep(5, 4), rep(7, 4)),
        rep(c(1, 3, 5, 7), 4), sep = '-')
    
    s <- 2 # skips first prime: 2
    
  } else { # modulo = 16
    combs <- paste(
      c(rep(1, 8), rep(3, 8), rep(5, 8), rep(7, 8), rep(9, 8), rep(11, 8), rep(13, 8), rep(15, 8)),
      rep(c(1, 3, 5, 7, 9, 11, 13, 15), 8), sep = '-')
    s <- 2 # skips first prime: 2
  }
  
  # creating result vector which will contain the distribution
  result <- numeric(length(combs))
  names(result) <- combs
  
  n <- length(last_digits)
  for (i in s:(n-1)) {
    # creates a string of pairs of last digits in the format "x-y" from row entries
    last_digit_pair <- paste(last_digits[i:(i + 1)], collapse = '-')
    # increments 'result' for the last digit pair x-y
    result[last_digit_pair] <- result[last_digit_pair] + 1
  }
  
  result
  
}
@

Based on the number basis (determined by the argument $modulo$) the final digit pairs $x - y$ are written as strings in the format $x - y$. A result vector is then created with these $x - y$ strings as index names, this is going to be used as our counter. A for loop is run and $primevector$ is traversed. For every pair of consecutive primes we increment the result vector where relevant.


\subsubsection{Distributions}

\paragraph{modulo 10}
<<freq1, echo=FALSE, eval=TRUE, fig=TRUE>>=
some_primes <- primes(1e6)

print(last_digit_pair_distr(some_primes, 10))
barplot(last_digit_pair_distr(some_primes, 10),
        main="Distribution of last digits mod 10", 
        xlab="Last digits", ylab="No. of Occurences", 
        font.lab=2, cex.main=1.8, cex.lab=1.3)
@

The distribution of consecutive primes' last digit pairs follow a pattern in number basis 10. There seems to be a correlation between the last digit and the frequency with which the digit pair occurs.

\paragraph{modulo 2}
<<freq2, echo=FALSE, eval=TRUE, fig=TRUE>>=
print(last_digit_pair_distr(some_primes, 2))
barplot(last_digit_pair_distr(some_primes, 2),
        main="Distribution of last digits mod 2",
        xlab="Last digits", ylab="No. of Occurences",
        font.lab=2, cex.main=1.8, cex.lab=1.3)
@

The distribution of consecutive primes' last digit pairs in number basis 2 is quite uneven. The reason for that is that the first two primes, 2 and 3 in number basis 2 will result in 0 and 1 as last digits, however, every following prime number will be odd, therefore their last digits will always be 1. 

\paragraph{modulo 8}
<<freq3, echo=FALSE, eval=TRUE, fig=TRUE>>=
print(last_digit_pair_distr(some_primes, 8))
barplot(last_digit_pair_distr(some_primes, 8),
        main="Distribution of last digits mod 8",
        xlab="Last digits", ylab="No. of Occurences", 
        font.lab=2,  cex.main=1.8, cex.lab=1.3)

@

The distribution of consecutive primes' last digit pairs follow a pattern in number basis 8. Here the chances to have the same last digits for two consecutive primes are approximately 50 \% less likely compared to any other combination of last digits.

\paragraph{modulo 16}
<<freq4, echo=FALSE, eval=TRUE, fig=TRUE>>=
print(last_digit_pair_distr(some_primes, 16))
barplot(last_digit_pair_distr(some_primes, 16),
        main="Distribution of last digits mod 16",
        xlab="Last digits", ylab="No. of Occurences", 
        font.lab=2, cex.main=1.8, cex.lab=1.3)
@

The distribution of consecutive primes' last digit pairs follow a pattern in number basis 16. There seems to be a correlation between the last digit and the frequency with which the digit pair occurs.

\end{document}











