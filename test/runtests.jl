using GSC
using Test

function g1()
    S = StrategySpace(3)
    Δ = [(i, i) for i in 1:3]
    u1(s, t) = (s, t[1]) in [Δ; (1, 2)]
    u2(s, t) = (s, t[1]) in [Δ; (3, 2)]
    p1 = Player(u1, S)
    p2 = Player(u2, S)
    return Game(p1, p2)
end

function g2()
    S = StrategySpace(4)
    u(s, t) = t[1] == 4 ? 0 : 4 - s
    return Game(i -> Player(u, S), 2)
end

@testset "Best replies" begin
    G = g1()

    s1, s2 = 1, 2
    t1, t2 = StrategyProfile(s1), StrategyProfile(s2)

    @test isbr(G[1], s1, t1) == true
    @test isbr(G[1], s2, t1) == false

    @test br(G[1], t1) == [s1]
    @test br(G[1], t2) == [s1, s2]
end

@testset "Brute force algorithm" begin
    G1 = g1()
    G2 = g2()

    e(i) = StrategyProfile(i, i)
    E1 = [e(i) for i in 1:3]
    E2 = [e(1), e(4)]

    @test solve(G1, BruteForce()) == E1
    @test solve(G2, BruteForce()) == E2
end

@testset "Iteratve IBR algorithm" begin
    G1 = g1()
    G2 = g2()

    e(i) = StrategyProfile(i, i)
    E1 = [e(i) for i in 1:3]
    E2 = [e(1), e(4)]

    @test issetequal(solve(G1, ItrIBR()), E1)
    @test issetequal(solve(G2, ItrIBR()), E2)
end
