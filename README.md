# Scala IBClient

This project is intended to be a re-implementation of Trader Workstation (TWS) API (Java version) with Scala 3 and the Typelevel stack.

The TWS API IBClient is copyright interactive brokers and subject to its license, see https://www.interactivebrokers.com/en/index.php?f=5041 under Trader Workstation (TWS) API or refer directly to https://interactivebrokers.github.io/tws-api/index.html

Since this project does not have any dependency (source code or binary) on the original, user of this project only requires reference to the original documentation and code base for clarification or latest updates.

Unlike the original Java project, which is a core-java project with no framework dependency, this project utilizes a few projects from Typelevel stack.

The main goal of this project is to demonstrate the functional programming paradigm with Scala 3 ecosystem on a practical scenario such as programmatic trading on the interactive broker platform.

# Motivation and trade off

Cats Effect is a library that provides an **effect system** for Scala, which allows you to write pure and composable code that can interact with the outside world¹. Core Java does not have an effect system, so you have to deal with side effects and impurity in your code.

Some possible benefits of using Cats Effect over Core Java are:

- You can use **IO** as a data type that represents a computation that may have an effect¹. IO is lazy and referentially transparent, which means it only runs when you evaluate it and it always produces the same result given the same input¹. This makes your code easier to reason about and test.
- You can use **Fiber** as a lightweight abstraction for concurrency¹. Fiber lets you run multiple IO computations in parallel without blocking threads¹. You can also cancel fibers gracefully and handle errors with combinators¹.
- You can use **Resource** as a way to manage resources that need to be acquired and released¹. Resource ensures that your resources are always cleaned up properly, even in the presence of errors or cancellation¹.
- You can use **Sync** and **Async** as type classes that abstract over different kinds of effects¹. Sync lets you lift any synchronous effect into IO, while Async lets you lift any asynchronous effect into IO¹. This gives you more flexibility and interoperability with other libraries.
- You can use **cats-effect-kernel**, **cats-effect-std**, and **cats-effect-testkit** as modules that provide common functionality for working with effects, such as concurrency primitives, streaming, testing, etc.²

Of course, using Cats Effect also comes with some trade-offs, such as:

- You need to learn a new paradigm and a new library, which may have a steep learning curve².
- You need to depend on an external library that may not be compatible with all versions of Scala or other libraries².

Ultimately, the choice of using Cats Effect or Core Java depends on your use case, preferences, and goals. 

Source of this section: Conversation with Bing with minor adjustment, 5/11/2023

(1) Tutorial · Cats Effect - Typelevel. https://typelevel.org/cats-effect/docs/tutorial.

(2) Introduction to Cats Effects | Baeldung on Scala. https://www.baeldung.com/scala/cats-effects-intro.

(3) Concepts · Cats Effect - Typelevel. https://typelevel.org/cats-effect/docs/concepts.


# Prerequisite

User of this project does require an interactive broker user account.
