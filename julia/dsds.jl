# # # ä = 1
# # ̈a
# # ̈s
# # ̈a  

# # aₘₐₓ
# x = 3
# # print(x, 'n')
# sᶿₖ
# ßₖ,

# struct Attribute

#     value::Real

#     @assert value <= 100
#     "Attribute values must be less or equal to the scales' upper bound, 100."

#     @assert value >= 0
#     "Attribute values must be greater or equal to the scales' lower bound, 0."

# end

struct ScaleBounds

    # parameters
    ub::Number
    lb::Number
    # ub::Float64
    # lb::Float64

    # scale object
    function ScaleBounds(upper, lower)

        # assert scale bounds
        @assert upper >= lower
        error("The scales' upper bound must be higher than its lower bound.")

        # new scale instance
        new(upper, lower)
    end

end

# mutable struct Attribute

#     value::Number
#     ub::Number
#     lb::Number

#     # normsb::Function = value -> (value - lb) / (ub - lb)

#     # bounds::ScaleBounds

#     # function Attribute(value)

#     #     @assert value <= 100
#     #     error("Attribute values must be less or equal to the scales' upper bound, 100.")

#     #     @assert value >= 0
#     #     error("Attribute values must be greater or equal to the scales' lower bound, 0.")

#     # end

# end

mutable struct lalala{T<:AbstractFloat} <: AbstractFloat

    x::T

end

lalala(1)
a = PositiveFloat(10)
a.x

mutable struct Attribute
    a::AbstractFloat
    ub::AbstractFloat
    lb::AbstractFloat

end

dsds = Attribute()

dsds.value
dsds.lb
dsds.ub

normsb(a::Attribute) =
    (a.value - a.lb) /
    (a.ub - a.lb)


aᵢᵏ = Attribute(100, 100, 0)

aᵢᵏ
aᵢᵏ.value
aᵢᵏ.ub
aᵢᵏ.lb

normsb(aᵢᵏ)

normsb(dsds)

normax(a::Vector{<:Number}) = a / maximum(a)

normax([100, 0])

normsb(a::Vector{<:Number}) =
    (a - minimum(a)) /
    (maximum(a) - minimum(a))


normsb([100, 0])


mutable struct SkillSet <: AbstractVector
    attributes::Vector{Attribute}
end

SkillSet([100, 0, 50])



# Attribute()