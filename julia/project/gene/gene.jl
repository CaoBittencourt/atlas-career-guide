# define generality for a skill set vector
gene(x::SkillSet) = mean(normsb(x))

# define generality for a skill set matrix
# apply gene to every skill set in the matrix
# gene(x::SkillMat)