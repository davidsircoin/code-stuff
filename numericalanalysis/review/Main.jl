module Utils

export section, subsection, text, forward_euler, rk2, simple_bcp, assemble_A, assemble_F

globallevel = 0

function section(msg)
    global globallevel = 0

    print("\n" ^3)
    print("#" ^ (length(msg) + 4) * "\n")
    print("# " * msg * " #" * "\n")
    print("#" ^ (length(msg) + 4) * "\n")
    print("\n")

end

function subsection(level, msg)
    global globallevel = level

    print("\n")
    print(" " ^ (globallevel * 2))
    print(msg)
    print("\n")
end

function text(msg)
    lines = split(msg, "\n")
    for line in lines
        print(" " ^ (globallevel * 2 + 2))
        print(line)
        print("\n")
    end
end

# Given a left and right boundary condition, and a range of x-values, calculates
# an approximate solution to our BVP: y''(x) + y(x) = 0.
#
# Returns a vector with y-values for each x-value.
function simple_bvp(leftbc, rightbc, xs::StepRangeLen{T}) where {T}
    A = assemble_A(xs)
    F = assemble_F(leftbc, rightbc, xs)

    # Here `\` should use LU-decomposition solve the equation Ax = F.
    # https://docs.julialang.org/en/v1/stdlib/LinearAlgebra/#Base.:\\-Tuple{AbstractMatrix,%20AbstractVecOrMat}
    return A \ F
end

# For our specific BVP, construct the matrix A in the equation Ax = F given
# a range coresponding to our discretized points
function assemble_A(xs::StepRangeLen{T}) where {T}

    n = length(xs)

    # stepsize
    h = convert(T, xs.step)

    A = zeros(n, n)
    A[1,1] = 1
    A[n,n] = 1
    for i in 2:(n-1)
        A[i, i-1] = h^-2
        A[i, i] = 1 - 2 * h^-2
        A[i, i+1] = h^-2
    end
    return A
end

# For out specific BVP, construct the matrix F in the equation Ax = F given
# the boundary condtions and a range coresponding to our discretized points
function assemble_F(leftbc, rightbc, xs::StepRangeLen{T}) where {T}
    n = length(xs)
    F = zeros(n)
    F[1] = leftbc
    F[n] = rightbc
    return F
end
end



module Part2

import Main.Utils: section, subsection, text, assemble_A, assemble_F
import LinearAlgebra

function main()
    section("Lab 5 part 2.1 Pen and paper tasks")
    text("See paperandpen.pdf for answers")

    section("Lab 5 part 2.2 LU-factorization for an ODE-BVP")



    ##############
    # Part 2.2.1 #
    ##############

    subsection(1, "1)")
    text("<nothing to say here>")



    ##############
    # Part 2.2.2 #
    ##############

    subsection(1, "2)")
    text("<nothing to say here>")



    ##############
    # Part 2.2.3 #
    ##############

    subsection(1, "3)")

    text("Running benchmark for \"naive\" approach...\n")

    xs = range(0, pi/2, 1002)
    A = assemble_A(xs)

    @time begin
        for t in range(1, 100, 100)
            leftbc = 0
            rightbc = 2*sin(pi*t)
            F = assemble_F(leftbc, rightbc, xs)

            A \ F
        end
    end

    text("\nDone!")



    ##############
    # Part 2.2.4 #
    ##############

    subsection(1, "4)")
    text("<noting to say here>")



    ##############
    # Part 2.2.5 #
    ##############

    subsection(1, "5)")
    text("Running benchmark for precomputed LU-decomposed matrix...\n")

    # `lu` performs LU-decomposition
    # and stores L, U and P in a struct with a nice interface so that we can
    # use it almost as if it was a single matrix
    LU = LinearAlgebra.lu(A)
    @time begin
        for t in range(1, 100, 100)
            leftbc = 0
            rightbc = 2*sin(pi*t)
            F = assemble_F(leftbc, rightbc, xs)

            LU \ F
        end
    end

    text("\nDone!")


    ##############
    # Part 2.2.6 #
    ##############

    subsection(1, "6)")
    text("""
        Under the hood `numpy.linalg.solve` uses the `_gesv` routine from LAPACK which
        is a library written in Fortran.

        I'm not sure but I think the documentation for `_gesv` is found here:
            https://netlib.org/lapack/explore-html/d7/d3b/group__double_g_esolve_ga5ee879032a8365897c3ba91e3dc8d512.html
        If so then that means that `numpy.linalg.solve` uses LU-decomposition with partial pivoting.
        """)
end
end



module Part1

export main

import Main.Utils: section, subsection, text, simple_bvp, assemble_A, assemble_F
import Plots: plotly, plot, plot!, savefig

function main()
    section("Lab 5 Part 1.1 Pen and paper tasks")
    text("See paperandpen.pdf for answers")



    section("Lab 5 Part 1.2 Coding tasks")

    plotly()

    y(x) = 2*sin(x) + cos(x)

    ##############
    # Part 1.2.1 #
    ##############

    xs_fine = range(0, pi/2, 20)
    xs_coarse = range(0, pi/2, 5) # h=pi/8 => #points=5 (3 + 2 boundary conditions)

    ys_analytical = map(y, xs_fine)
    ys_aprox = simple_bvp(1, 2, xs_coarse)

    myplot = plot(xaxis="x", yaxis="y")
    plot!(myplot, xs_fine, ys_analytical, label="Analytical")
    plot!(myplot, xs_coarse, ys_aprox, label="Aproximated N=3")

    savefig(myplot, "./plot_1_2_1.html")

    subsection(1, "1)")
    text("Plot for this task saved to: plot_1_2_1.html\n")

    ##############
    # Part 1.2.2 #
    ##############

    A = assemble_A(xs_coarse)
    F = assemble_F(1, 2, xs_coarse)

    subsection(1, "2)")
    text("""
        For N=3, y(0) = 1 and y(pi/2) = 2 we have the equation

            Ax = F

        with A =""")
    display(A)

    text("\nand F=")
    display(F)

    text("\nwhich is consistent with what we found by paper and pen.\n")




    ##############
    # Part 1.2.3 #
    ##############

    subsection(1, "3)")
    text("""
        The error appears to be biggest in the middway point between the two boundary conditions.
        The local truncation error of the centered difference is dominated by the third derivative
        which in this case is it's biggest at the ends of the intervall, contradicting what we see in
        the graph. I guess there is some interplay between these two dynamics; closer
        to the boundaries => more accurate, the 3rd derivative is big => less acurate.

        This differs from an IVP where the error cascades and is it's biggest at the end of the intervall.
        """)

    ##############
    # Part 1.2.4 #
    ##############

    ns = 5:500
    global_errors = zeros(length(ns))
    for (i, n) in enumerate(ns)
        xs = range(0, pi/2, n)
        ys = map(y, xs)
        ys_aprx = simple_bvp(1, 2, xs)

        local_errors = map(abs, ys .- ys_aprx)
        global_errors[i] = sum(local_errors) / n
    end

    stepsizes = map((n) -> (pi/2)/n, ns)

    myplot = plot(xaxis = "h", yaxis="Global error", scale=:log)
    plot!(myplot, stepsizes, global_errors, label="BVP with centered difference")
    plot!(myplot, stepsizes, map((h) -> h^2, stepsizes), label="y=h^2")

    savefig(myplot, "plot_1_2_4.html")

    subsection(1, "4)")
    text("""
        Plot for this task saved to: plot_1_2_4.html

        Looking at the graph we see that the global error decreeses quadraticly
        with respect to the step-size h, meaning that the globl error from this
        experimental observation would be O(h^2) which is consistent with the
        theoretical truncation error.

        The global error for an approximation at stepsize h was calculated by
        taking the difference between each approximated discretization point and
        analytical value, then summing them and dividing by the total number of points.
        """)

    ##############
    # Part 1.2.5 #
    ##############

    subsection(1, "5)")
    text("""
        In the supplied python-snippet the last line `np.linalg.solve` is
        the command doing the actual heavy lifting, everything before that is just setup.

        I don't know what the implementation of `np.linalg.solve` is, here in Julia
        I'm using `A \\ F` to solve the euation Ax = F which uses LU-decomposition.
        """)

end
end



module Part3

import Main.Utils: section, subsection, text, assemble_A, assemble_F
import LinearAlgebra: Diagonal
import Plots: plotly, plot, plot!, savefig

function main()
    section("Lab 3 Part 3")



    ############
    # Part 3.1 #
    ############

    subsection(1, "1)")
    text("I have skipped this part per Josefin's instructions.")



    ############
    # Part 3.2 #
    ############

    subsection(1, "2)")
    text("The function for the Jacobi-method is implemented in `jacobi_meyhod`")



    ############
    # Part 3.3 #
    ############

    subsection(1, "3)")

    leftbc = 1
    rightbc = 2
    N = 5
    initialguess = ones(N+2)
    xs = range(0, pi/2, N+2)

    A = assemble_A(xs)
    F = assemble_F(leftbc, rightbc, xs)

    iterative = jacobi_method(A, initialguess, F, 100)
    direct = A \ F

    text("The aproximated solution using the jacobi-method is:\n")
    display(iterative)
    text("\nUsing the direct method from lab 5 part 1 we obtain:\n")
    display(direct)
    text("\nThese results are very similar!\n")



    ############
    # Part 3.4 #
    ############

    subsection(1, "4)")

    plotly()
    myplot = plot(xaxis="X", yaxis="Y")

    for iterations in [10, 20, 30, 40, 50]
        iterative = jacobi_method(A, initialguess, F, iterations)
        myplot = plot(myplot, xs, iterative, label="Jacobi $iterations iterations")
    end

    myplot = plot(myplot, xs, direct, label="Direct")

    savefig(myplot, "./plot_3_4.html")
    text("Saved a plot to plot_3_4.html\n")
    text("I'd say that the the approximation starts becomming vissibly inacruate at about 40 iterations.\n")



    ############
    # Part 3.5 #
    ############

    subsection(1, "5)")

    N = 10
    initialguess = ones(N+2)
    xs = range(0, pi/2, N+2)
    A = assemble_A(xs)
    F = assemble_F(leftbc, rightbc, xs)

    direct = A \ F

    plotly()
    myplot = plot(xaxis="X", yaxis="Y")

    for iterations in [10, 20, 30, 40, 50, 100]
        iterative = jacobi_method(A, initialguess, F, iterations)
        myplot = plot(myplot, xs, iterative, label="Jacobi $iterations iterations")
    end

    myplot = plot(myplot, xs, direct, label="Direct")

    savefig(myplot, "./plot_3_5.html")
    text("Saved a plot to plot_3_5.html\n")
    text("For N=10 significantly more iterations are needed to obtain a good approximation.\n")


end

function jacobi_method(A, x0, B, n)
    D = Diagonal(A)
    Di = inv(D)
    LU = - A + D

    x = x0
    for i in 1:n
        x = Di * (LU) * x + Di * B
    end

    return x
end
end



module Main

import Main.Part1
import Main.Part2
import Main.Part3

function main()
    Main.Part1.main()
    Main.Part2.main()
    Main.Part3.main()
end
end
