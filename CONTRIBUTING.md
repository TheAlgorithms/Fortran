# Contributing Guidelines

Welcome to [TheAlgorithms/Fortran](https://github.com/TheAlgorithms/Fortran)!

Welcome to Fortran! This repository is meant to be referenced and used by learners worldwide, and we aspire to maintain the highest possible quality of the code presented here! If you have any questions or concerns about this guide, please feel free to [state them clearly in an issue](https://github.com/TheAlgorithms/Fortran/issues/new) or ask the community in [Discord](https://the-algorithms.com/discord).

## Table Of Contents

* [What is an Algorithm?](#what-is-an-algorithm)
* [Contributor agreement](#contributor-agreement)
* [Contribution guidelines](#contribution-guidelines)
  + [Implementation requirements](#implementation-requirements)
  + [Fortran Coding Style](#Fortran-coding-style)
    - [Readability and naming conventions](#readability-and-naming-conventions)
    - [Compilation](#compilation)
    - [Types](#types)
    - [Exceptions and side-effects](#exceptions-and-side-effects)
    - [Documentation, examples, and tests](#documentation-examples-and-tests)
    - [Other](#other)
  + [Minimal example](#Minimal-example)
  + [Submissions Requirements](#submissions-requirements)

## What is an Algorithm?

An Algorithm is one or more functions that:

- take one or more inputs,
- perform some internal calculations or data manipulations,
- return one or more outputs,
- have minimal side effects (Examples of side effects: `print`, `read`).

## Contributor Agreement

Being one of our contributors, you agree and confirm that:

- Your work will be distributed under [MIT License](LICENSE) once your pull request is merged.
- Your work meets the standards of this guideline.

## Contribution Guidelines

We appreciate any contribution, from fixing a grammar mistake in a comment to implementing complex algorithms. Please check the [directory](DIRECTORY.md) and [issues](https://github.com/TheAlgorithms/Fortran/issues/) for an existing (or declined) implementation of your algorithm and relevant discussions.

**New implementations** are welcome! This includes new solutions for a problem, different representations for a data structure, and algorithm design with different complexity or features.

**Improving documentation and comments** and **adding tests** is also highly welcome.

**Identical implementations** are not allowed.

### Environment
The environment that I am using is `GNU Fortran (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0`, a quite stable version of Fortran. Lower versions of Fortran are also acceptable.

### Implementation requirements

- The unit of implementation we expect is a [**Fortran Module**](https://fortran-lang.org/en/learn/best_practices/modules_programs/). Although the main goals of this repository are educational, the module form mirrors a real-world scenario and makes it easy to use the code from this repository in other projects.
- The first line must contain the canonical title of the module prefixed by double hashes (`## Title Of The Module`). This title is used in this repository's automation for populating the [Directory](DIRECTORY.md).
- The module should be thoroughly documented with doc comments. Follow the [Fortran documentation style](https://dftbplus-develguide.readthedocs.io/en/latest/fortranstyle.html#comments).
- The file begins with the module-level documentation with the general description and explanation of the algorithm/data-structure:
  * Any restrictions of the implementation and any constraints for the input data.
  * An overview of the use cases.
  * Recommendations for when to use or avoid using it.
  * Comparison with the alternatives.
  * Links to source materials and further reading.
- Use intuitive and descriptive names for objects, functions, and variables.
- Return all calculation results instead of printing or plotting them.
- Avoid importing third-party libraries. Only use those for complicated algorithms and only if the alternatives of relying on the standard library or including a short amount of the appropriately-licensed external code are not feasible.
### Fortran Coding Style

#### Readability and naming conventions

We want your work to be readable by others; therefore, we encourage you to follow the official [Fortran Coding Style](https://fortran-lang.org/en/learn/best_practices/style_guide/#naming-convention).

- Help your readers by using **descriptive names** that eliminate the need for redundant comments.
- Avoid single-letter variable names, unless it has a Minimal lifespan. If your variable comes from a mathematical context or no confusion is possible with another variable, you may use single-letter variables. Generally, single-letter variables stop being OK if there are more than just a couple of them in scope. Some examples:
  * Prefer `index` or `idx` to `i` for loops.
  * Prefer `src` and `dst` to `a` and `b`.
  * Prefer `remainder` to `r` and `prefix` to `p`.
- Expand acronyms. Prefer `greatest_common_divisor()` to `gcd()`, as the former is easier to understand than the latter, especially for non-native English speakers.

### Minimal example

```Fortran
!> My Algorithm
!!
!! Description, explanation, recommendations, sources, links.

!! This simple program adds two numbers
program addNumbers

   implicit none

!! Type declarations
   real :: a, b, result

!! Executable statements
   a = 12.0
   b = 15.0
   result = a + b
   print *, 'The total is ', result

end program addNumbers
```

### Submissions Requirements

- Make sure the code compiles before submitting.
- Look up the name of your algorithm in other active repositories of [TheAlgorithms](https://github.com/TheAlgorithms/), like [TheAlgorithms/Python](https://github.com/TheAlgorithms/Python). By reusing the same name, your implementation will be appropriately grouped alongside other implementations on the [project's website](https://the-algorithms.com/).
- Please help us keep our issue list small by adding fixes: Add the number of the issue you solved — even if only partially — to the commit message of your pull request.
- Use *snake_case* (words separated with an underscore `_`) for the filename.
- Try to fit your work into the existing directory structure as much as possible. Please open an issue first if you want to create a new subdirectory.
- Writing documentation, be concise, and check your spelling and grammar.
- Add a corresponding explanation to [Algorithms-Explanation](https://github.com/TheAlgorithms/Algorithms-Explanation) (optional but recommended).
- Most importantly, **be consistent in the use of these guidelines**.

**Happy coding!**

---

Authors: [@SatinWuker](https://github.com/SatinWuker)
