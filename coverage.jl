using Pkg

# Ensure Coverage is available (it's a dev-only dependency)
try
    using Coverage
catch
    @info "Installing Coverage.jl (dev-only dependency)..."
    Pkg.add("Coverage")
    using Coverage
end

"""
    generate_coverage_report()

Generate test coverage data and HTML report for RBNF.
"""
function generate_coverage_report()
    # Run tests with coverage
    println("Running tests with coverage...")
    cd(@__DIR__) do
        Pkg.test("RBNF"; coverage=true)
    end

    # Process coverage data
    println("\nProcessing coverage data...")
    source_dir = joinpath(@__DIR__, "src")
    coverage_data = process_folder(source_dir)

    # Print coverage summary
    println("\n" * "="^60)
    println("COVERAGE SUMMARY")
    println("="^60)

    # Calculate overall coverage
    total_lines = 0
    covered_lines = 0

    for file_coverage in coverage_data
        filename = file_coverage.filename
        # Get coverage counts for each line
        counts = file_coverage.coverage
        # Filter out nothing values and only count valid lines
        valid_counts = filter(x -> x !== nothing, counts)
        total = length(valid_counts)
        covered = count(x -> x > 0, valid_counts)

        if total > 0  # Only include files with valid coverage data
            total_lines += total
            covered_lines += covered

            coverage_pct = (covered / total) * 100
            rel_path = replace(filename, @__DIR__() * "/" => "")
            println("  $(rpad(rel_path, 30)) $(round(coverage_pct, digits=1))% ($covered/$total lines)")
        end
    end

    coverage_percent = (covered_lines / total_lines) * 100
    println("\n  $(rpad("TOTAL", 30)) $(round(coverage_percent, digits=2))% ($covered_lines/$total_lines lines)")
    println("="^60)

    # Generate LCOV info file
    println("\nGenerating LCOV data...")
    lcov_file = joinpath(@__DIR__, "coverage", "lcov.info")
    mkpath(dirname(lcov_file))

    open(lcov_file, "w") do io
        LCOV.write(io, coverage_data)
    end

    # Generate HTML report using lcov
    println("Generating HTML report...")
    html_dir = joinpath(@__DIR__, "coverage", "public")
    mkpath(html_dir)

    run(`genhtml $(lcov_file) -o $(html_dir) --quiet`)

    println("\n✓ HTML report generated in $(html_dir)/")
    println("✓ Open $(html_dir)/index.html in your browser")

    return coverage_data
end

# Run if this script is executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    generate_coverage_report()
end
