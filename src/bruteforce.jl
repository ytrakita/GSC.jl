"""
    BruteForce()

Return an instance of the brute force algorithm.
"""
struct BruteForce <: GSCAlgorithm end

function (::BruteForce)(G::Game{N}) where {N}
    E = StrategyProfile{N}[]

    for idx in CartesianIndices(size(G))
        ss = StrategyProfile(i -> G[i].S[idx[i]], N)
        iseqm(G, ss) && push!(E, ss)
    end

    return E
end
