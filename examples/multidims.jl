using GSC

import GSC: StrategySpace, Player, Game

struct Params{T<:Vector{<:Real}}
    θ::T
    N::Int
    K::Int
    d::Int
end

StrategySpace(p::Params) = StrategySpace(i -> Base.OneTo(p.K), p.d)

function Player(i::Int, p::Params)
    (; θ, N, K, d) = p
    S = StrategySpace(p)
    v(x, z) = (2 / (N - 1)) * x * z / K^2 - (x / K)^θ[i]
    D::Int = d
    u(s, t) = sum(v(s[l], sum(t)[l]) for l in 1:D)
    u(s::Int, t) = v(s, sum(t))
    return Player(u, S)
end

Game(p::Params) = Game(i -> Player(i, p), p.N)

function simulate(N, K, d)
    θ = rand(N) + 1.5 * ones(N)
    p = Params(θ, N, K, d)
    G = Game(p)

    println("")
    @time e1 = solve(G, BruteForce())
    @time e2 = solve(G, ItrIBR())
    @show issetequal(e1, e2), length(e1)

    return
end

function main()
    R = 10
    N = 3
    K = 100
    d = 1
    for i in 1:R
        simulate(N, K, d)
    end
    return
end
