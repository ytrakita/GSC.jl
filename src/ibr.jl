_br(p::Player, t::StrategyProfile, dom) = argmax(s -> p(s, t), dom)

minbr(p::Player, t::StrategyProfile) = _br(p, t, p.S)
maxbr(p::Player, t::StrategyProfile) = _br(p, t, reverse(p.S))

minbr(p::Player, t::StrategyProfile, s::Strategy) =
    _br(p, t, restrmin(p.S, s))
maxbr(p::Player, t::StrategyProfile, s::Strategy) =
    _br(p, t, reverse(restrmax(p.S, s)))

_br(f::F, G::Game{N}, ss::StrategyProfile{N}, restr::Bool) where {N,F} =
    StrategyProfile(i -> begin
        @inbounds restr ? f(G[i], ss[-i], ss[i]) : f(G[i], ss[-i])
    end, N)

minbr(G::Game{N}, ss::StrategyProfile{N}; restr::Bool=false) where {N} =
    _br(minbr, G, ss, restr)
maxbr(G::Game{N}, ss::StrategyProfile{N}; restr::Bool=false) where {N} =
    _br(maxbr, G, ss, restr)

"""
    IBR <: GSCAlgorithm

Abstract type for the iterative best reply algorithm.
"""
abstract type IBR <: GSCAlgorithm end

function _itrbr(f, ibr::T, G::Game) where {T<:IBR}
    (; ss, maxitr) = ibr
    cnt = 0
    ssnew = f(G, ss)
    while ss != ssnew
        cnt == maxitr && error("IBR didn't converge!")
        cnt += 1
        ss = ssnew
        ssnew = f(G, ss; restr=true)
    end
    return ss
end

"""
    IminBR(ss::StrategyProfile, maxitr=1000)

Return an instance of the iteratve infimum best reply algorithm from initial
state `ss`. The value of `maxitr` is the maximum number of iteration.
"""
@kwdef struct IminBR <: IBR
    ss::StrategyProfile
    maxitr::Int=1000
end

(ibr::IminBR)(G::Game) = _itrbr(minbr, ibr, G)

"""
    ImaxBR(ss::StrategyProfile, maxitr=1000)

Return an instance of the iteratve supremum best reply algorithm from initial
state `ss`. The value of `maxitr` is the maximum number of iteration.
"""
@kwdef struct ImaxBR <: IBR
    ss::StrategyProfile
    maxitr::Int=1000
end

(ibr::ImaxBR)(G::Game) = _itrbr(maxbr, ibr, G)

_addonehot(s, d::Int) = s + (1:length(s) .== d)
_addonehot(s::Int, ::Int) = s + 1

_addonehot(ss::StrategyProfile{N}, i::Int, d::Int) where {N} =
    StrategyProfile(j -> begin
        @inbounds j == i ? _addonehot(ss[j], d) : ss[j]
    end, N)

function iseqm(
    G::Game,
    mnew::StrategyProfile,
    ssbr::StrategyProfile,
    ssmn::StrategyProfile
)
    for (j, p) in enumerate(G)
        (; S) = p
        @inbounds ls, us, t = ssbr[j], ssmn[j], mnew[-j]
        @inbounds payoff = p(mnew[j], t)
        for s in S
            isgeq(s, ls) && !isgeq(s, us) || continue
            payoff >= p(s, t) || return false
        end
    end
    return true
end

function transit!(
    H::Vector{<:StrategyProfile{N}},
    E::Vector{<:StrategyProfile{N}},
    M::Vector{<:StrategyProfile{N}},
    G::Game{N},
    ibrmaxitr::Int,
) where {N}
    Mnew = StrategyProfile{N}[]
    h = StrategyProfile{N}[]
    for m in M
        ssbr = minbr(G, m)
        for i in 1:N
            @inbounds (; S) = G[i]
            for d in 1:ndims(S)
                @inbounds m[i][d] < maximum(S)[d] || continue
                ssmn = _addonehot(m, i, d)
                ssmn in h && continue
                push!(h, ssmn)
                mnew = IminBR(ss=ssmn, maxitr=ibrmaxitr)(restrmin(G, ssmn))
                (mnew in Mnew || mnew in H) && continue
                push!(Mnew, mnew)
                push!(H, mnew)
                !(mnew in E) && iseqm(G, mnew, ssbr, ssmn) && push!(E, mnew)
            end
        end
    end
    return Mnew
end

"""
    ItrIBR(maxitr=1000, ibrmaxitr=1000)

Return an instance of the iterative IBR algorithm. The maximum number of
iteration is `maxitr`, and the maximum number of iteration for each IBR in the
iteration is `ibrmaxitr`.
"""
@kwdef struct ItrIBR <: GSCAlgorithm
    maxitr::Int=1000
    ibrmaxitr::Int=1000
end

function (itribr::ItrIBR)(G::Game)
    (; maxitr, ibrmaxitr) = itribr
    ssmin, ssmax = minimum(G), maximum(G)

    emin = IminBR(ss=ssmin, maxitr=ibrmaxitr)(G)
    emin == ssmax && return [emin]

    emax = ImaxBR(ss=ssmax, maxitr=ibrmaxitr)(restrmin(G, emin))
    emin == emax && return [emin]

    cnt = 0
    G = restr(G, emin, emax)
    M, H, E = [emin], [emin, emax], [emin, emax]
    while !isempty(M)
        cnt == maxitr && error("Iterative IBR didn't converge!")
        cnt += 1
        M = transit!(H, E, M, G, ibrmaxitr)
    end

    return E
end
