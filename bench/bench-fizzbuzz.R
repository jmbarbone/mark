library(mark, warn.conflicts = FALSE)
library(bench)

x <- 1:1e5

foo1.1 <- function(x) {
  out <- as.character(x)
  out[x %% 3 == 0] <- "Fizz"
  out[x %% 5 == 0] <- "Buzz"
  out[x %% 15 == 0] <- "FizzBuzz"
  out
}

foo1.2 <- function(x) {
  out <- as.character(x)
  three <- x %% 3 == 0
  five <- x %% 5 == 0
  out[three] <- "Fizz"
  out[five] <- "Buzz"
  out[three & five] <- "FizzBuzz"
  out
}

foo2.1 <- function(x) {
  out <- character(length(x))

  for (i in seq_along(x)) {
    if (x[i] %% 3 == 0) {
      if (x[i] %% 5 == 0) {
        out[i] <- "FizzBuzz"
      } else {
        out[i] <- "Fizz"
      }
    } else if (x[i] %% 5 == 0) {
      out[i] <- "Buzz"
    } else {
      out[i] <- x[i]
    }
  }

  out
}

foo2.2 <- function(x) {
  out <- character(length(x))

  for (i in seq_along(x)) {
    if (x[i] %% 15 == 0) {
      out[i] <- "FizzBuzz"
    } else if (i %% 3 == 0) {
      out[i] <- "Fizz"
    } else if (i %% 5 == 0) {
      out[i] <- "Buzz"
    } else {
      out[i] <- x[i]
    }
  }

  out
}

foo2.3 <- function(x) {
  out <- character(x)

  for (i in x) {
    if (i %% 3 == 0) {
      if (i %% 5 == 0) {
        i <- "FizzBuzz"
      } else {
        i <- "Fizz"
      }
    } else if (i %% 5 == 0) {
      i <- "Buzz"
    }
  }

  out
}

foo3.1 <- function(x) {
  vapply(
    x,
    function(i) {
      if (i %% 3 == 0) {
        if (i %% 5 == 0) {
          "FizzBuzz"
        } else {
          "Fizz"
        }
      } else if (i %% 5 == 0) {
        "Buzz"
      } else {
        as.character(i)
      }
    },
    character(1)
  )
}

foo3.2 <- function(x) {
  vapply(
    x,
    function(i) {
      if (i %% 15 == 0) {
        "FizzBuzz"
      } else if (i %% 3 == 0) {
        "Fizz"
      } else if (i %% 5 == 0) {
        "Buzz"
      } else {
        as.character(i)
      }
    },
    character(1)
  )
}

foo4.0 <- function(x) {
  # only works when ordered
  n <- length(x)
  out <- as.character(x)

  out[seq.int(0, n, by = 3)] <- "Fizz"
  out[seq.int(0, n, by = 5)] <- "Buzz"
  out[seq.int(0, n, by = 15)] <- "FizzBuzz"

  out
}

n <- length(x)
bench::mark(
  fizzbuzz(n),
  fizzbuzz_lazy(n), # For the win
  foo1.1(x),
  foo1.2(x),
  foo2.1(x),
  foo2.2(x),
  foo3.1(x),
  foo3.2(x),
  foo4.0(x),
  iterations = 10
)
