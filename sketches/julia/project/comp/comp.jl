#SECTION: competence function 
#region: regular notation 

using Statistics

mutable struct SkillSet
	a::Vector{Number}
	ub::Number
	lb::Number
end

dsds = SkillSet([100, 0, 50], 100, 0)

typeof(dsds)

dsds.a
dsds.lb
dsds.ub

normsb(aₖ::SkillSet) = (aₖ.a .- aₖ.lb) / (aₖ.ub - aₖ.lb)

# define competence for a skill set vector
comp(aₖ::SkillSet) = mean(normsb(aₖ))
comp(dsds)

# define competence for a skill set matrix
# apply comp to every skill set in the matrix
# comp(x::SkillMat)

#endregion
#region: turse notation 

# define competence for a skill set vector
c(aₖ::SkillSet) = mean(normsb(aₖ))

# define competence for a skill set matrix
# apply comp to every skill set in the matrix
# comp(x::SkillMat)

#endregion
