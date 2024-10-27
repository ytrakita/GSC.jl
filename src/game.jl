# Strategies

"""
    Strategy

Alias for `Union{Int,Vector{Int}}`.
"""
const Strategy = Union{Int,Vector{Int}}

isgeq(s::T, t::T) where {T<:Strategy} = all(s .>= t)

"""
    StrategyRange

Alias for `AbstractUnitRange{<:Int}`.
"""
const StrategyRange = AbstractUnitRange{<:Int}

"""
    StrategySpace((R1, R2...))
    StrategySpace(R1, R2...)

Construct a strategy space whose range of dimension `i` is `Ri`.

# Examples
```jldoctest
julia> StrategySpace(1:3, 1:5)
3×5 StrategySpace{2, Tuple{UnitRange{Int64}, UnitRange{Int64}}}
 1:3
 1:5
```
"""
struct StrategySpace{N,R<:NTuple{N,StrategyRange}} <: AbstractArray{Strategy,N}
    indices::R
end

StrategySpace(R::StrategyRange...) = StrategySpace(Tuple(R))

"""
    StrategySpace(n::Int...)

Construct a strategy space whose range of each dimension is `1:n`.

# Examples
```jldoctest
julia> StrategySpace(3)
3-element StrategySpace{1, Tuple{Base.OneTo{Int64}}}
 Base.OneTo(3)

julia> StrategySpace(2, 3)
2×3 StrategySpace{2, Tuple{Base.OneTo{Int64}, Base.OneTo{Int64}}}
 Base.OneTo(2)
 Base.OneTo(3)
```
"""
StrategySpace(n::Int...) = StrategySpace(map(Base.OneTo, n))

"""
    StrategySpace(f::Function, n::Integer)

Construct an `n`-dimensional strategy space, computing the range of each
dimension as `f(i)`, where `i` is the index of the dimension.

# Examples
```jldoctest
julia> StrategySpace(i -> 2 * i, 3)
2×4×6 StrategySpace{3, Tuple{Base.OneTo{Int64}, Base.OneTo{Int64}, Base.OneTo{Int64}}}
 Base.OneTo(2)
 Base.OneTo(4)
 Base.OneTo(6)
```
"""
StrategySpace(f::F, n::Integer) where F = StrategySpace(ntuple(f, n))

Base.show(io::IO, S::StrategySpace{N}) where {N} =
    print(io, "StrategySpace{$N}(", S.indices, ")")

function Base.show(io::IO, ::MIME"text/plain", S::StrategySpace{N}) where {N}
    println(io, summary(S))
    for (i, v) in enumerate(S.indices)
        print(" ", v)
        i == N && break
        print(io, "\n")
    end
end

Base.size(S::StrategySpace) = map(length, S.indices)

Base.getindex(S::StrategySpace{N,R}, I::Vararg{Int,N}) where {N,R} =
    [map(getindex, S.indices, I)...]

Base.IndexStyle(::Type{<:StrategySpace{1}}) = IndexLinear()

Base.getindex(S::StrategySpace{1}, i::Int) =
    1 <= i <= length(S) ? S.indices[1][i] : throw(BoundsError(S, i))

restrmin(S::StrategySpace, s::T) where {T<:Strategy} =
    StrategySpace(Tuple(UnitRange.(s, maximum(S))))
restrmax(S::StrategySpace, s::T) where {T<:Strategy} =
    StrategySpace(Tuple(UnitRange.(minimum(S), s)))

restrmin(S::StrategySpace{1}, s::Int) = StrategySpace(s:maximum(S))
restrmax(S::StrategySpace{1}, s::Int) = StrategySpace(minimum(S):s)

"""
    StrategyProfile((s1, s2...))
    StrategyProfile(s1, s2...)

Construct a strategy profile in which `i`th player's strategy is `si`.
"""
struct StrategyProfile{N,T<:NTuple{N,Strategy}} <: AbstractVector{Strategy}
    data::T
end

StrategyProfile(s::Strategy...) = StrategyProfile(Tuple(s))

"""
    StrategyProfile(f::Function, n::Integer)

Construct an `n`-player strategy profile, computing the strategy of each
player as `f(i)`, where `i` is the index of the player.

# Examples
```jldoctest
julia> StrategyProfile(i -> 2 * i, 3)
(2, 4, 6)
```
"""
StrategyProfile(f::F, n::Integer) where F = StrategyProfile(ntuple(f, n))

Base.show(io::IO, ss::StrategyProfile) = print(io, ss.data)
Base.show(io::IO, ::MIME"text/plain", ss::StrategyProfile{N}) where {N} =
    print(io, ss.data)

Base.size(::StrategyProfile{N}) where {N} = (N,)

Base.IndexStyle(::Type{<:StrategyProfile}) = IndexLinear()

function Base.getindex(ss::StrategyProfile{N}, i::Int) where {N}
    1 <= abs(i) <= N || throw(BoundsError(ss, i))
    i < 0 && return StrategyProfile(j -> ss[j + (j >= -i)], N - 1)
    return ss.data[i]
end

isgeq(x::T, y::T) where {N,T<:StrategyProfile{N}} = all(isgeq.(x, y))

restr(x, mn::T, mx::T) where {T<:Union{Strategy,StrategyProfile}} =
    restrmax(restrmin(x, mn), mx)

# Player

"""
    Player(u::Function, S::StrategySpace)

Construct a player whose payoff function is `u` and strategy space is `S`.
The first argument of `u` is own strategy and the second is opponents' strategy
profile.

# Examples
```jldoctest
julia> p = Player((s, t) -> s * t[1] - s, StrategySpace(2))
Player{1}((Base.OneTo(2),))
```
"""
struct Player{N,F<:Function}
    u::F
    S::StrategySpace{N}
end

Base.show(io::IO, p::Player{N}) where {N} =
    print(io, "Player{$N}(", p.S.indices, ")")

(p::Player)(s::T, t::StrategyProfile) where {T<:Strategy} = p.u(s, t)

restrmin(p::Player, s::T) where {T<:Strategy} = Player(p.u, restrmin(p.S, s))
restrmax(p::Player, s::T) where {T<:Strategy} = Player(p.u, restrmax(p.S, s))

# Game

"""
    Game((p1, p2...))
    Game(p1, p2...)

Construct a game consisting of players `p1`, `p2`, ....
"""
struct Game{N} <: AbstractVector{Player}
    players::NTuple{N,Player}
end

Game(p::Player...) = Game(Tuple(p))

"""
    Game(f::Function, n::Integer)

Construct an `n`-player game, computing each player as `f(i)`, where `i` is the
index of the player.
"""
Game(f::F, n::Integer) where F = Game(ntuple(f, n))

Base.show(io::IO, G::Game{N}) where {N} =
    print(io, "Game{$N}(", G.players, ")")

Base.size(G::Game) = map(length, getfield.(G.players, :S))
Base.length(::Game{N}) where {N} = N

Base.IndexStyle(::Type{<:Game}) = IndexLinear()

function Base.getindex(G::Game{N}, i::Int) where {N}
    1 <= i <= N || throw(BoundsError(G, i))
    return G.players[i]
end

Base.axes(::Game{N}) where {N} = (Base.OneTo(N),)

Base.minimum(Ss::T) where {N,T<:NTuple{N,StrategySpace}} =
    StrategyProfile(map(minimum, Ss))
Base.minimum(G::Game) = minimum(getfield.(G.players, :S))

Base.maximum(Ss::T) where {N,T<:NTuple{N,StrategySpace}} =
    StrategyProfile(map(maximum, Ss))
Base.maximum(G::Game) = maximum(getfield.(G.players, :S))

restrmin(G::Game{N}, ss::StrategyProfile{N}) where {N} =
    Game(i -> restrmin(G[i], ss[i]), N)
restrmax(G::Game{N}, ss::StrategyProfile{N}) where {N} =
    Game(i -> restrmax(G[i], ss[i]), N)

# best replies

"""
    isbr(p::Player, s::Strategy, t::StrategyProfile)

Return `true` if `s` is a best reply of player `p` to `t`; `false` otherwise.
"""
isbr(p::Player, s::T, t::StrategyProfile) where {T<:Strategy} =
    p(s, t) == maximum(x -> p(x, t), p.S)

"""
    br(p::Player, t::StrategyProfile)

Return the vector of all best replies of player `p` to `t`.
"""
br(p::Player, t::StrategyProfile) = p.S[findall(s -> isbr(p, s, t), p.S)]

# equilibrium

"""
    iseqm(G::Game, ss::StrategyProfile)

Return `true` if `ss` is a Nash equilibrium in game `G`; `false` otherwise.
"""
function iseqm(G::Game{N}, ss::StrategyProfile{N}) where {N}
    for i in 1:N
        @inbounds isbr(G[i], ss[i], ss[-i]) || return false
    end
    return true
end
