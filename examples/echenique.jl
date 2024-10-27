using GSC

import GSC: Player, Game

struct Params{T<:Vector{<:Real}}
    α::T
    β::T
    K::Int
end

function Player(i::Int, p::Params)
    (; α, β, K) = p
    S = StrategySpace(K)
    u(s, t) = begin
        s = s / K
        t = t[1] / K
        c = -(α[i] / 10) * (s - t)^2 + 200 * β[i] * sin(100 * s)
        d = ((1 - α[i]) * s * (1 + t) - (1 / 2 - β[i]) * s^2 / 100)
        return c + d / 100
    end
    return Player(u, S)
end

Game(p::Params) = Game(i -> Player(i, p), 2)

function simulate(K)
    α = rand(2)
    β = rand(2)
    p = Params(α, β, K)
    G = Game(p)

    println("")
    @time e1 = solve(G, BruteForce())
    @time e2 = solve(G, ItrIBR())
    @show issetequal(e1, e2), length(e1)

    return
end

function main()
    R = 10
    K = 1000
    for i in 1:R
        simulate(K)
    end
    return
end
