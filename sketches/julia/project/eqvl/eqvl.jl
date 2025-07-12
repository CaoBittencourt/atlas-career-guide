include("/home/Cao/Storage/github/atlas-research/julia/SkillSet.jl")
using SkillSet.jl

# generalized logistic function
function logistic(
	x,
	a = 0,
	k = 1,
	c = 1,
	q = 1,
	m = 0,
	b = 1,
	ν = 1
)

	return a + (k - a) / ((c + q * exp(-b * (x - m)))^(1 / ν))

end

# baseline linear-logistic function
# eq(x::Vector{Number}, m::Percent) =

# equivalence family of functions
function aeq(x::SkillSet)

	# normalize attributes by max
	â = normax(x)

	# calculate skill set generality
	γ = gene(x)

	# calculate attribute equivalence
	ä = logistic(
		x = â,
		a = 0,
		k = â,
		c = 1,
		q = γ * (1 - â),
		m = γ,
		b = 1 / (1 - γ),
		ν = â / γ
	)

	return ä

end

# aeq(x::SkillSet) =
#     logistic(
#         x=x,
#         m=gene(x),
#         a=0,
#         k=x,
#         c=1,
#         q=m * (1 - x),
#         ν=x / m,
#         b=1 / (1 - m)
#     )
# eq(x::SkillSet) = aeq(x)

eq(x::Similarity) = seq(x)

eq(x::EduExp) = eed(x)