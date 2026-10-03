
# Table of Contents

1.  [Contributing to Plauna](#org3a305ee)
    1.  [Ways to Contribute](#orgc8f18cd)
        1.  [Feature suggestions](#orgfa4da83)
        2.  [Design improvements](#org94d4d26)
        3.  [Code contributions](#org9a1792c)
        4.  [Documentation contributions](#org7847b73)
    2.  [How to Contribute](#orgc9fa868)
        1.  [1) Check existing issues](#orgbca835b)
        2.  [2) Start a discussion](#orgfa005ac)
        3.  [3) Fork and create a branch](#orgcd81996)
        4.  [4) Make your changes](#org630e2eb)
        5.  [5) Test and verify](#orgc4ae4db)
        6.  [6) Open a Pull Request](#orgf071605)
2.  [Contribution guidelines](#org5f177f3)
    1.  [Code style and quality](#org38c1368)
    2.  [Documentation](#org8d9515f)
    3.  [Feature/design proposals](#orgc8abc1c)
3.  [Reporting bugs](#org7d4e01e)
4.  [LICENSE](#orgbd790b2)


<a id="org3a305ee"></a>

# Contributing to Plauna

Thanks for your interest in contributing! Contributions are welcome and can take many forms, including code, documentation improvements, feature suggestions, and design enhancements.


<a id="orgc8f18cd"></a>

## Ways to Contribute


<a id="orgfa4da83"></a>

### Feature suggestions

-   Propose new capabilities
-   Request enhancements to existing behavior
-   Share ideas based on real-world usage


<a id="org94d4d26"></a>

### Design improvements

-   Improve UX/UI or interaction flows
-   Suggest visual or usability enhancements
-   Provide design reviews and rationale


<a id="org9a1792c"></a>

### Code contributions

-   Fix bugs
-   Improve performance or reliability
-   Add new features
-   Refactor or improve existing code


<a id="org7847b73"></a>

### Documentation contributions

-   Fix typos and broken links
-   Improve clarity, structure, and examples


<a id="orgc9fa868"></a>

## How to Contribute


<a id="orgbca835b"></a>

### 1) Check existing issues

Before starting, please look at open issues to avoid duplicate work.

-   If you don’t see an issue for your idea/bug, create one.


<a id="orgfa005ac"></a>

### 2) Start a discussion

If your contribution is a larger change (new feature, major refactor, big UX update), please open an issue first to align on scope and direction.


<a id="orgcd81996"></a>

### 3) Fork and create a branch

Create a branch for your change:

    git checkout -b feature/your-change-name


<a id="org630e2eb"></a>

### 4) Make your changes

Follow the project’s existing style and conventions.

If you’re adding code, consider:

keeping changes focused
writing or updating tests when applicable
adding documentation for user-facing behavior


<a id="orgc4ae4db"></a>

### 5) Test and verify

Run the project’s test/lint commands:

    clj-kondo --lint src         # linting
    cljfmt fix src test          # code style
    clj -M:test                  # testing


<a id="orgf071605"></a>

### 6) Open a Pull Request

When ready:

-   Open a PR from your branch into the main branch
-   Use a clear title and description
-   Explain what changed and why
-   Link any related issues


<a id="org5f177f3"></a>

# Contribution guidelines


<a id="org38c1368"></a>

## Code style and quality

-   Keep code readable and consistent with existing patterns.
-   Prefer small, reviewable commits.
-   Avoid unrelated formatting changes unless necessary.


<a id="org8d9515f"></a>

## Documentation

-   Update docs when your change affects user behavior.
-   Include screenshots or examples when it helps explain the change.


<a id="orgc8abc1c"></a>

## Feature/design proposals

When proposing a feature or design change, please include:

-   the problem you’re solving
-   the proposed solution
-   expected impact/benefits
-   alternatives considered (if any)


<a id="org7d4e01e"></a>

# Reporting bugs

If you encounter a bug:

-   open an issue
-   include steps to reproduce, expected vs actual behavior, and relevant logs/screenshots


<a id="orgbd790b2"></a>

# LICENSE

By contributing, you agree that your contributions will be licensed under the project’s license.

