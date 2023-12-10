struct Range
    destinationStart::Int64
    sourceStart::Int64
    length::Int64
end

function readInput(filename::String)::Tuple{Vector{Int64},Vector{Vector{Range}}}
    lines = readlines(filename)
    seeds = map(w -> parse(Int64, w), split(lines[1])[2:end])

    # add empty line at the end
    push!(lines, "")

    transforms = []
    acc::Vector{String} = []
    for i in eachindex(lines)[3:end]
        if lines[i] == ""
            push!(transforms, [Range([parse(Int64, w) for w in split(l)]...) for l in acc[2:end]])
            acc = []
        else
            push!(acc, lines[i])
        end
    end

    return (seeds, transforms)
end

function convert(x::Int64, ranges::Vector{Range})::Int64
    for range in ranges
        if range.sourceStart <= x < range.sourceStart + range.length
            return x + range.destinationStart - range.sourceStart
        end
    end
    return x
end

function convertBack(x::Int64, ranges::Vector{Range})::Int64
    for range in ranges
        if range.destinationStart <= x < range.destinationStart + range.length
            return x - (range.destinationStart - range.sourceStart)
        end
    end
    return x
end

function calculateDiscontinuities(ranges::Vector{Range})::Vector{Int64}
    [n for r in ranges for n in [r.sourceStart - 1, r.sourceStart, r.sourceStart + r.length, r.sourceStart + r.length + 1]]
end

function partOne(seeds::Vector{Int64}, transforms::Vector{Vector{Range}})::Int64
    locations = []
    for s in seeds
        for t in transforms
            s = convert(s, t)
        end
        push!(locations, s)
    end

    minimum(locations)
end

function partTwo(seeds::Vector{Int64}, transforms::Vector{Vector{Range}})
    discontinuities = []
    for t in reverse(transforms)
        discontinuities = map(x -> convertBack(x, t), discontinuities)
        append!(discontinuities, calculateDiscontinuities(t))
    end

    discontinuities = sort(unique(discontinuities))

    inputs::Vector{Int64} = []
    for i in 1:2:length(seeds)
        start = seeds[i]
        length = seeds[i+1]

        append!(inputs, filter(x -> start <= x < start + length, discontinuities))
        push!(inputs, start)
        push!(inputs, start + length)
    end

    partOne(inputs, transforms)
end

seeds, transforms = readInput("../input.txt")
println("Part One: ", partOne(seeds, transforms))
println("Part Two: ", partTwo(seeds, transforms))
