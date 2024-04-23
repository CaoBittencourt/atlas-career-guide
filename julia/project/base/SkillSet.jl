# skill set struct
mutable struct SkillSet <: AbstractVector
	x::Vector{Attribute}
end

# skill set methods
normsb(x::SkillSet) = (x - x.lb) / (x.ub - x.lb)

normax(x::SkillSet) = (x - x.lb) / (x.ub - x.lb)

gene(x::SkillSet) = mean(normax(x))

comp(x::SkillSet) = mean(normsb(x))

eq(x::SkillSet) = aeq(x)


# turse skill set methods
# x̃ = normsb(x)

ä = eq(x::SkillSet)

c()

γ(x::SkillSet) = gene(x::SkillSet)