An (experimental) optimising compiler for TypeScript/JavaScript.

TypeScript is transpiled to JavaScript, which is minified and optimised.

## Purpose

Current compilers are limited to fairly basic optimisations, such as whitespace removal, basic inlining, basic dead code removal, etc. Many classical optimisation techniques are tricky to implement for JavaScript due to its dynamic nature, or simply haven't been attempted - only one compiler I'm aware of uses control flow or advanced type inference, for example.

That compiler is the [Google Closure Compiler](https://github.com/google/closure-compiler), which is in a league of its own in terms of the optimisations it can perform. It has two major problems however:

1. The largest issue is that the advanced optimisations rely on Closure's type annotations, requiring programmers to heavily rewrite their code to take advantage of Closure's full potential. These annotations are, by today's standards, niche, verbose, and have very little usage compared to e.g. TypeScript. Additionally, the type system is not sound and relies on the types provided by the programmer, with many assumptions baked into the optimisation passes, potentially resulting in mis-compiles. Without these type annotations, the Closure compiler is limited to simple optimisations, and underperforms other compilers such as [Terser](https://github.com/terser/terser).
2. Speed - today's JavaScript tooling ecosystem has a strong focus on speed, and the [Closure compiler can be quite slow, even when only performing simple optimisations](https://github.com/privatenumber/minification-benchmarks/).

The goal of this project is to create a compiler for JavaScript that produces output that is at least as good as the Closure compiler, while not requiring type annotations/code changes to reach that level of output, and being more performant.

The core architecture is based on [SWC](https://github.com/swc-project/swc/), with many of the optimisation passed based on the Closure compiler.

The main novelty of this project is the [points-to](https://en.wikipedia.org/wiki/Pointer_analysis) analysis, which is able to soundly track values through JavaScript's dynamic shenanigans without any type annotations (at least in theory - there are some bugs to fix). This opens the door to advanced optimisations, similar to Closure compiler, such as advanced inlining and property renaming.

The compiler also generates control flow graphs and performs data flow analysis. 

## Current status
I'm currently working on:
- Fixing bugs in the points-to analysis that result in mis-compiles.
- Extending the points-to analysis to more JavaScript constructs, such as classes. Currently the analysis bails out on quite a few constructs and labels their content as 'unknown', preventing them from being optimised.
- Porting other optimisation passes over from Closure compiler and extending them.

In its current state, the compiler is largely a playground for me to explore novel optimisations and analyses. However, I'd like to eventually turn it into a real-world tool. The main blockers for that are:
- Adding support for multiple input files/modules, and adapting the analyses to understand them. This is quite a large task, but is necessary because many advanced optimisations are severely inhibited when they only operate on one file at a time.
- Fixing the mis-compiles resulting from incorrect points-to analysis.
