---
title: Programming Language Technology
subtitle: DAT151 (Chalmers) / DIT231 (GU)
date: Winter Term 2025 (LP2)
---

<link rel="stylesheet" href="gh-fork-ribbon.css" />
<style>.github-fork-ribbon:before { background-color: #333; }</style>
<a class="github-fork-ribbon" href="https://github.com/teach-plt/www" data-ribbon="Sources on GitHub" title="Sources on GitHub">Sources on GitHub</a>

[Canvas](https://chalmers.instructure.com/courses/36705) /
[GitLab](https://git.chalmers.se/courses/dat151) /
[Schedule (TimeEdit)](https://cloud.timeedit.net/chalmers/web/public/ri10794Q057Z05Q66065Y755yZ0Q6W9759562Q5.html) /
[Slack](https://plt2025.slack.com/) ([Join Slack!](https://join.slack.com/t/plt2025/shared_invite/zt-3hapcdtfn-fToy857Q_ELojF7F3Q5WEQ)) /
[Chalmers studieportal](https://www.student.chalmers.se/sp/course?course_id=40144) /
[GU ad (sv)](https://www.gu.se/studera/hitta-utbildning/programspraksteknik-dit231) /
[GU kursplan](http://kursplaner.gu.se/pdf/kurs/sv/DIT231) /
[GU course description](http://kursplaner.gu.se/pdf/kurs/en/DIT231) /
[Course page 2022](http://www.cse.chalmers.se/edu/year/2022/course/DAT151/)

# Schedule

Lectures are Tuesdays and Thursdays, starting at **13:15**, both on
campus in
**[HC2](http://maps.chalmers.se/#23e2df72-cbb9-4aff-825a-1a282e9618d6)**
and in
[Zoom](https://chalmers.instructure.com/courses/36705/external_tools/419).
**No lecture nor labs on Tue 18 Nov due to [DatE-IT](https://date-it.se/).**

Material: plt = course book, dragon = Dragon book. Slides follow closely the plt book.

| Date | Time | Who   | Title | Lecture material | Reading material |
|------|------|-------|-------|------------------|------------------|
| Tue 04/11 | 13-15 | [AA] | PL Design, Compilation Phases |  | [slides](plt-book/ipl-book/slides/1-slides-ipl-book.pdf), plt 1, dragon 1, [git](notes/git-primer.html) |
| Thu 06/11 | 13-15 | [AA] | Grammars / BNFC / Hands-on with Lab 1 | live coding [start](live/2025/live-1-Calc-bnfc-start.zip), [result](live/2025/live-1-Calc-bnfc.zip) | [slides](plt-book/ipl-book/slides/2-slides-ipl-book.pdf), plt 2, dragon 2.8.2,4.1-4.3 |
| Tue 11/11 | 13-15 | [AA] | Formal languages, lexing, and parsing | [shift-reduce parsing](notes/sr-states.md), [LR-table](notes/LR-table.html), [lr-demo](https://github.com/teach-plt/lr-demo) | [slides](plt-book/ipl-book/slides/3-slides-ipl-book.pdf), plt 3, dragon 3,4 |
| Thu 13/11 | 13-15 | [MM] | Type checking | [script](notes/type-checking.html), [prime.c](notes/prime.c), [prime-stms.c](notes/prime-stms.c), [division.c](notes/division.c), [division-annotated.c](notes/division-annotated.c) | [slides](plt-book/ipl-book/slides/4-slides-ipl-book.pdf), plt 4, dragon 5,6 |
| *Mon 17/11* | *23* |     | *Lab 1 deadline* |  |  |
| Tue 18/11 | 10-17 |      | [DatE-IT](https://date-it.se/), no lecture, no labs |  |
| Thu 20/11 | 13-15 | [MM] | Interpreting | [script](notes/interpreter.html) | [slides](plt-book/ipl-book/slides/5-slides-ipl-book.pdf), plt 5 |
| Tue 25/11 | 13-14 | [MM] | Hands-on with Lab 2 (Haskell) | [live code](live/2025/lab2-live-haskell.zip) | [script: monadic interpreter](notes/monads.html) |
| Tue 25/11 | 14-15 | [MM] | Hands-on with Lab 2 (Java) | [script](notes/java.html), [Annotated.java](notes/Annotated.java) |  |
| Thu 27/11 | 13-15 | [MM] | Code generation | [script](notes/compilation.html), [prime.c](notes/prime.c), [prime.j](notes/prime.j) | [slides](plt-book/ipl-book/slides/6-slides-ipl-book.pdf), plt 6, dragon 6,7 |
| Tue 02/12 | 13-14 | [MM] | Hands-on with Lab 3 (Haskell) | [live code snippet](live/2025/lab3-live-haskell.hs) |  |
| Tue 02/12 | 14-15 | [MM] | Hands-on with Lab 3 (Java)   | [live code snippet](live/2025/lab3-live-java.java) |  |
| *Wed 03/12* | *23* |     | *Lab 2 deadline* |  |
| Thu 04/12 | 13-15 | [AA] | Functional programming languages | [script](notes/cbn-cbv.html) | [slides](plt-book/ipl-book/slides/7-slides-ipl-book.pdf), plt 7, dragon 6.5,7.3 |
| Tue 09/12 | 13-15 | [AA] | Type inference and polymorphism | [script](notes/typing.html), [visualization tool](https://github.com/teach-plt/visualize-type-inference) | plt 7.7-9 |
| Thu 11/12 | 13-14 | [AA] | Hands-on with Lab 4 (Haskell) | [live code](live/2025/lab4/Interpreter.hs) |  |
| Thu 11/12 | 14-15 | [AA] | Hands-on with Lab 4 (Java)    | [live code](live/2025/lab4/Interpreter.java) |  |
| Tue 16/12 | 13-15 | [AA] | Dependent types (Agda) | live coding [start](live/2025/agda/JVM-start.agda), [result](live/2025/agda/JVM.agda) |  |
| *Wed 17/12* | *23* |     | *Lab 3 deadline* |  |  |
| Thu 18/12 | 13-15 | [AA] | Preparing for the exam | last year's [exam](exams/exam-plt-2025-1.pdf), live [solution](live/2025/exam.md) |  |

|    2026     |               |                      |                   |
|:-----------:|---------------|----------------------|-------------------|
| *Mon 12/01* | *23*          | *Lab 4 deadline*     |                   |
| *Thu 15/01* | *8.30-12.30*  | *Exam*               |                   |
| *Fri 23/01* | *23*          | *Final lab deadline* | *all lab returns* |
| *Wed 28/01* | *14.30-15.30* | *Exam review*        | EDIT 6128         |
| *Mon 31/03* | *23*          | *Reexam lab deadline*|                   |
| *Thu 09/04* | *8.30-12.30*  | *First reexam*       |                   |
| *Thu 27/08* | *14-18*       | *Second reexam*      |                   |

The official course schema is in
[Time Edit](https://cloud.timeedit.net/chalmers/web/public/ri10794Q057Z05Q66065Y755yZ0Q6W9759562Q5.html).

# Description

The aim of the course is to give understanding of how programming languages are designed, documented, and implemented.
The course covers the basic techniques and tools needed to write interpreters, and gives a summary introduction to compilation as well.
Those who have passed the course should be able to

1.  define the lexical structure of programming languages by using
    regular expressions, explain the functioning of finite automata, and
    implement lexical analysers by using standard tools;

2.  define the syntax of programming languages by using context-free
    grammars, explain the principles of LL and LR parsing, and implement
    parsers by using standard tools;

3.  define and implement abstract syntax;

4.  master the technique of syntax-directed translation and its
    efficient implementation in their chosen programming language;

5.  formulate typing rules and implement type checkers;

6.  formulate operational semantic rules and implement interpreters;

7.  write simple code generators;

8.  be familiar with the basic implementation issues of both imperative
    and functional languages;

9.  master the principles of polymorphic type checking by unification;

10. implement an interpreter for a functional language.

# Teachers

- [Andreas Abel][AA] ([homepage](https://www.cse.chalmers.se/~abela/)), responsible course teacher and examiner.
- [Magnus Myréen][MM] ([homepage](https://www.cse.chalmers.se/~myreen/)), course teacher.

[AA]: https://www.chalmers.se/en/persons/abela/
[MM]: https://www.chalmers.se/en/persons/myreen/

Assistants:

- [András Kovács](https://www.chalmers.se/personer/andrask/)
- [David Wärn](https://www.chalmers.se/personer/warnd/) ([homepage](https://dwarn.se))
- [Jeremy Pope](https://www.chalmers.se/en/persons/popje/)
- [Jonas Höfer](https://www.chalmers.se/personer/hoferj/)

Questions regarding this class (organization, content, labs) should be asked publicly on the Slack forum in the most cases.
You are also welcome to answer questions by others.
Do not give away any lab solutions when you ask or answer questions!

# Lab supervision

Lab supervision is offered in room
[ED3354](http://maps.chalmers.se/#45ea04d0-4a90-4a18-9842-55599dbbb093)
and online on Tue, Thu and Fri.
Starting 12 Nov we offer Zoom-only supervision on Wednesdays 15:15-17:00.
The lab rooms and supervision are available from Tue 04 Nov till Fri 19 Dec 2025.
Attendance is voluntary.

| Day |    Time     | Location     | Supervisors            |
|:---:|:-----------:|--------------|------------------------|
| Tue | 15:15-17:00 | Zoom, ED3354 | Andreas, David, Jonas  |
| Wed | 15:15-17:00 | Zoom         | Jeremy, Jonas          |
| Thu | 15:15-17:00 | Zoom, ED3354 | Magnus, Andras         |
| Fri | 13:15-15:00 | Zoom, ED3354 | Andras, Jeremy, David  |

Extra Zoom-only supervision session will be provided:
- Wed 7 Jan 2026 15:15-17:00 (Jeremy, Jonas)
- Thu 8 Jan 2026 15:15-17:00 (Andras, David)

We use [Slack](https://plt2025.slack.com/) to organize the lab supervision. [Please join](https://join.slack.com/t/plt2025/shared_invite/zt-3hapcdtfn-fToy857Q_ELojF7F3Q5WEQ) our Slack workspace and the channel `#queue`.

The TAs will be present in lab rooms during lab supervision slots, available for help both in-person and virtually.
To ask for help, just send a ticket request through the `#queue` Slack channel.

- For in-person attendance, write your name and how to find you (room if not ED3354, number of the PC in front of you (or any other hint how to recognize you)).

- For online help, start a Zoom meeting and invite your group partner to join.
  Please then sign up by providing a clickable URL to your Zoom meeting (e.g.,
  `https://chalmers.zoom.us/j/6435657890/pwd=OXlBcGxMZjkzNGsyplpYZENYWlVodi09`),
  so that the TA knows where to find you.

- Monitor the queue, the TA will join you/your Zoom meeting as soon as it’s your turn.


# Labs

You have to pass the labs to pass the course.
However, the course grade is determined solely by the exam.

- Lab 1 - parser (deadline 17/11)
- Lab 2 - type checker and interpreter (deadline 03/12)
- Lab 3 - code generator (deadline 17/12)
- Lab 4 - functional language interpreter (deadline 11/01/2026)

The labs are quite substantial, so please set aside at least 30 full working hours (4 full working days) before the deadline.
It is recommended to start at least 10 days before the deadline.

Labs are to be solved in **groups of two**. (Individual solutions are accepted per exception, please contact the course responsible.)
You are expected to find a lab partner with whom you will do the labs.
If you have difficulties finding a partner, please use Slack channel `#lab-partner`.
Groups are formed on Canvas and then recreated automatically on
[Chalmers GitLab](https://git.chalmers.se/courses/dat151).
After the first lab has been submitted, the groups are fixed. (Should you nevertheless need to urgently change to group, please contact the course responsible.)

The labs will be published in your Gitlab group and a solution repository will be created for you there.
Submission of your solution is by creating a `submission` tag in the repository.
Please read the detailed **lab instructions** at:
<https://chalmers.instructure.com/courses/36705/pages/lab-infrastructure-on-chalmers-gitlab>

## Lab grading

We guarantee two gradings per lab: one for the version submitted before
the ordinary deadline for that lab, the other for a resubmission before
the final deadline.
If your first submission does not build or does not pass the testsuite, you will just get *fails testsuite* as grading.

As part of the grading, you may be asked to explain your solution in person to a course teacher.
Be prepared to get a call for such an explanation meeting.
In particular, make sure you understand all parts of the solution (good documentation helps!).

## Lab confidentiality

Keep your lab solutions confidential!

- If you post problems and discussions around the labs on Canvas etc., make sure you do not give away the solution.
- Publishing the solution on the internet (e.g. in a GitHub repo) is **forbidden**.
- *Even after the course is completed*, publishing remains forbidden, because labs are reused every year.

## AI policy

**Mild use of AI** is permitted when solving the labs.
For example:

- You may ask a chatbot to explain some concept or library functions to you.
- You may use CoPilot (or similar) for code completion in a modest way.
- You may ask CoPilot (or simliar) to refactor your code.

**Heavy use of AI** (such as _vibe coding_) is not permitted.
Heavy use of AI jeopardizes your learning experience.
It can be regarded as implicit plagiarism from the authors of the code the AI was trained on.
Some examples of heavy AI use:

- Feeding the lab description into an AI chat (e.g. ChatGPT) and using the produced code.
- Writing a comment and let AI implement the method/function.

In any case, you need to disclose AI use in your submission.
If you used AI, write a comment explaining the extent of its use:

- What tools did you use?  (E.g. GitHub Copilot for completion, Copilot chat, AI chat XYZ (name the model!))
- What were they used for?  (E.g. advice on libraries, advice on the problem, refactoring, completing definitions...)
- Any definitions written with AI help should be labeled as such in a comment.


# Exam

The written exam determines the course grade, the usual grading scales apply:
Chalmers: 5, 4, 3, U;
Gothenburg University: VG, G, U.

Exam dates: 15 Jan 2026 am J, 9 Apr 2026 am J, 27 Aug 2026 pm J.

The exam tests the understanding of the course contents from a more high-level view, e.g., the underlying theoretical concepts.
The exam has the same structure as these [old exams](exams/)
(download as [archive](exams.tgz)).

Further, here are some old
[exercises](plt-book/ipl-book/course-exercises/exx.html) and
[solutions](plt-book/ipl-book/course-exercises/sol.html) to prepare for the exam.

# Literature

1. The main book will be one that developed from earlier editions of this course:

   Aarne Ranta,
   *Implementing Programming Languages. An Introduction to Compilers and Interpreters*,
   College Publications, London, 2012.
   [Web page (with extra material and links to selling sites)](plt-book/ipl-book/)

   Please also check the
   [errata](https://github.com/andreasabel/plt-errata/) page
   (welcome to submit errata not covered there yet).

2. If you are really interested in the topic, for instance, if you want to
   continue with the
   [Compiler Construction](http://www.cse.chalmers.se/edu/course/TDA283/)
   course, you should also consider *the Dragon book*,

   Aho, Lam, Sethi & Ullman,
   *Compilers Principles, Techniques & Tools*, **Second edition**,
   Pearson/Addison Wesley 2007.

Both books are available at web bookshops. The main book will also be sold at Cremona.

A good (yet slightly dated) introduction to monads in Haskell, useful
for implementing interpreters, type checkers, and compilers, is this
article:

Philip Wadler,
[Monads for functional programming](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf).
In *Advanced Functional Programming*,
First International Spring School on Advanced Functional Programming Techniques,
Båstad, Sweden, May 24-30, 1995.

It also contains an introduction to parser combinators.

# Software

To solve the labs, you need a developer environment with the following tools.

## General tools

You need to invoke tools from a [command shell](https://en.wikipedia.org/wiki/Shell_(computing)).
For one, the  [make](https://en.wikipedia.org/wiki/Make_(software)) build tool
and the [git](https://git-scm.com/) version control tool are required.

- On **Linux**, these are available by default.
- On **macOS**, you can install them from Xcode (`xcode-select –-install`).
- On **Windows**, you should install [Git for Windows](https://git-scm.com/downloads/win). This installs Git Bash, a command line shell that mimics Linux and has `make` and `git`. You should generally just work from inside Git Bash in the rest of the course.

### Setting `PATH` and other environment variables

Tools can be invoked from the shell only if they are in the [`PATH`](https://en.wikipedia.org/wiki/PATH_(variable))
of your command shell.
You can add directories to the search path by setting the `PATH` variable in the initialization script of your shell.  Such scripts are located in your `$HOME` directory.  The name of the initialization script usually contains the name of the shell.
- E.g. for the Bash: `.bashrc` or `.bash_profile`
- E.g. for the Zsh: `.zshrc`

Appending the line `export PATH=/absolute/path/to/dir:${PATH}` to the initialization script will add directory `/absolute/path/to/dir` to the front of the `:`-separated list of search paths.
- On Windows, this list is `;`-separated because `:` is reserved for drive names.
  Windows also allows to set environment variables globally for a user through the Control Panel.

Note that updates to the initialization script only take effect when the shell is restarted.


## Haskell tools

Recent versions of the following Haskell tools need to be installed and in your `PATH`.

- [Haskell Stack](https://docs.haskellstack.org/en/stable/), e.g. version 3.7.1
- The Haskell compiler [GHC](https://www.haskell.org/ghc/) version 9.6.7
- BNFC, the [BNF Converter](https://bnfc.digitalgrammars.com/), e.g. version 2.9.6
- Haskell lexer generator [Alex](https://haskell-alex.readthedocs.io/en/stable/)
- Haskell parser generator [Happy](https://haskell-happy.readthedocs.io/en/stable/)

We suggest the following installation.

1. First install [GHCup](https://www.haskell.org/ghcup/).
   - When asked, answer Yes to adding the ghcup installation directory to PATH.
   - Also answer Yes to installing `stack`.
   - On Windows: copy the install script from the GHCup webpage into PowerShell. The installation might take a long time (10+ minutes), apparently because of the Windows Defender antivirus checking the installation very slowly. After this is finished, work in Git Bash for all of the following steps.
2. You can use `ghcup tui` to review your installed versions of Haskell tools and to install/uninstall them. After installing a tool, you have to "set" it to make it visible in your shell. Install and set the latest Stack version, and also GHC 9.6.7.
3. Use Stack to install the remaining tools.
   ```
   stack install alex happy BNFC
   ```
   This might alert you in the end that you do not have the installation directory in your system `PATH`; in this case, go and add it there.

4. Verify that these tools are working by querying their version:
   ```
   stack --version
   ghc   --version
   bnfc  --version
   alex  --version
   happy --version
   ```

## Java

You need the [java](https://en.wikipedia.org/wiki/Java_(programming_language)) virtual machine in your `PATH`.
```
java -version
```
We will use version 21 of Java.

You might chose to solve the labs in Java.
In this case, you need the Java Development Kit (JDK). You can get all neccessary Java dependencies by installing `openjdk-21` (e.g. with a package manager on Linux/macOS or by downloading [here](https://learn.microsoft.com/en-us/java/openjdk/download) on Windows).
We publish Java stubs for the labs that use [Gradle build tool](https://gradle.org/), preconfigured to install the Java parser generator [CUP](http://www2.cs.tum.edu/projects/cup/) and the lexer generator [JFLex](https://jflex.de/).  If you use the stubs, there is not need to install Gradle, CUP and JFlex yourself.

# Student representatives

[Student representatives](https://student.portal.chalmers.se/en/chalmersstudies/courseinformation/courseevaluation/Pages/default.aspx)
for DAT151 Programming language technology.

| Program | @student.chalmers.se |     Name     |
|:-------:|:--------------------:|:------------:|
| MPALG  | ali0408mousa@gmail.com   | Ali Mousa Baqer Al-Muslim |
| MPSOF  | aoxinyan@gmail.com       | Xinyan Ao                 |
| MPALG  | matistjati@outlook.com   | Joshua Bergman Andersson  |
| MPCSN  | albin.hallin1@gmail.com  | Albin Hallin              |
| UTBYTE | ajyarmus@uwaterloo.ca    | Aubrey Yarmus             |

<hr>
<p>
<a href="https://teach-plt.github.io/www/">https://teach-plt.github.io/www/</a>, (C) Andreas Abel 2025
</p>
