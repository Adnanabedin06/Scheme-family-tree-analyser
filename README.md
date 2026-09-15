# Scheme Family Tree Analyser

A functional programming project developed in Scheme using DrRacket. The application stores and analyses structured maternal and paternal family-tree data through recursive functions, list processing and reusable data-query operations.

## Project Overview

This project demonstrates how functional programming techniques can be used to process nested family records. Each record contains a family member’s name, parents, birth date and optional death date. The program performs a range of queries and transformations without relying on object-oriented structures.

## Main Features

* Lists members from the maternal and paternal family trees
* Combines both branches into a complete family list
* Identifies members with recorded parents
* Finds the oldest living member in each family branch
* Calculates the average age at death
* Filters members by their birthday month
* Sorts members alphabetically by first name
* Renames matching family members through data transformation
* Processes structured records using recursion, mapping and filtering

## Technologies Used

* **Scheme** — primary programming language
* **DrRacket** — development and execution environment
* **Functional programming** — program design approach
* **Git and GitHub** — version control and project hosting

## Running the Project

1. Download or clone this repository.
2. Open either `.rkt` file in DrRacket.
3. Click **Run** to execute the program and view the results.

## File Structure

```text
Scheme-family-tree-analyser/
├── family-tree-analyser.rkt
├── family-tree-analyser-partB.rkt
├── README.md
└── .gitignore
```

* **`family-tree-analyser.rkt`** — contains the family-tree datasets and core analysis functions, including member listing, filtering, sorting and age calculations.
* **`family-tree-analyser-partB.rkt`** — contains the second component of the project and its additional family-tree processing functions.
* **`README.md`** — provides the project documentation and instructions.
* **`.gitignore`** — prevents unnecessary generated files from being tracked.

## What I Learned

Through this project, I strengthened my understanding of functional programming and recursive problem-solving. I gained practical experience working with nested data structures, decomposing larger requirements into reusable functions and using operations such as `map`, `filter`, `sort`, `cons`, `car` and `cdr`.

The project also improved my ability to test, debug and document code within DrRacket while managing source files through GitHub.

## Author

**Adnan Abedin**
Final-year Computer Science and Cybersecurity student at the University of Greenwich.
