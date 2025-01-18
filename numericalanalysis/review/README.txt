---- Message for students doing peergrade on this assignment ----

💖💖💖💖💖💖💖💖💖💖💖💖💖
💖 It's your lucky day  💖
💖💖💖💖💖💖💖💖💖💖💖💖💖

I have implemented this hand-in in a programming language called Julia, not Python. Julia is a relatively new programming language designed with scientific computing in mind, it aims to be as readable a Python, but with a speed aproaching that of Fortran and other compiled languages. I've spoken to the course-leader about this and gotten permission to do the hand-ins in Julia, but you as a student are not expected to peer grade these hand-ins, so you can grade this assignment randomly and use one of the questions to flag to the TAs that this is in Julia and they will grade it instead.

Have a nice day ✨🐍✨



---- Message for TAs --------------------------------------------

The assignment is in Main.jl and has been written using Julia 1.8.5 (built 2023-01-08)

To run the entire assignment:

0) Be in the same directory as `Main.jl`
1) Enter the REPL: `julia --project=.`
2) Install required packages: `using Pkg; Pkg.instantiate()`
3) Run the assignment: `include("Main.jl"); Main.main()`

Step (3) will likely take some time since all of the dependencies have to
be JIT-compiled before running.

🛑 IMPORTANT 🛑
When running the benchmarks in part 2 the results can be heavily skewed as code might be compiled _inside_ the benchmark (JIT-compilation).
To deal with this run the code twice: one time to compile everything and then again for the actual benchmark.

If you have trouble running the code I've also saved all the plots inside the `pregenerated-plots` directory.

Have a nice day ✨🦀✨
