abstract type GSCAlgorithm end

function iseqm(G::Game{N}, e::StrategyProfile, z::StrategyProfile) where {N}
    n = length(e)
    for i in 1:(N - n)
        @inbounds isbr(G[n + i], z[i], StrategyProfile(e..., z[-i]...)) ||
            return false
    end
    return true
end

"""
    solve(G::Game, algo::Algo, r=0) where {Algo<:GSCAlgorithm}

Return the vector of all Nash equilibria of `G` by algorithm `algo`. If `r` is
set positive, `r`th order recursive version of the algorithm is used.
"""
function solve(G::Game{N}, algo::Algo, r::Int=0) where {N,Algo<:GSCAlgorithm}
    r == 0 && return algo(G)
    1 <= r <= N - 1 || throw(BoundsError(G, r))

    n = N - r
    E = StrategyProfile{N}[]

    for idx in CartesianIndices(size(G)[n + 1:N])
        z = StrategyProfile(i -> G[n + i].S[idx[i]], r)
        g = Game(i -> begin
            u(s, t) = G[i](s, StrategyProfile(t..., z...))
            return Player(u, G[i].S)
        end, n)
        for e in algo(g)
            iseqm(G, e, z) && push!(E, StrategyProfile(e..., z...))
        end
    end

    return E
end
