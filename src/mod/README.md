# Atlas Career Guide Constructs and Modules
This is a brief description of statistical methods and models in the `src/mod` directory.

## Summary Table
|id|symbol|construct|input|output|class|category|subcategory|module|submodule|
|--|------|---------|-----|------|-----|--------|-----------|------|---------|
|1|$\tilde{a}$|maxima-normalized skill set|1 skill set|1 skill set|descriptive models|basic|attribute|-|-|
|3|$\gamma$|skill set generality|1 skill set|1 number|descriptive models|generality|-|`describe`|`gene`|
|2|$\ddot{a}$|attribute equivalence|1 skill set|1 skill set|descriptive models|equivalence|attribute|`describe`|`eqvl`|
|4|$c$|skill set competence|1 skill set|1 number|descriptive models|competence|-|`describe`|`comp`|
|5|$\hat{g}$|estimated intelligence|1 fa-informed subset of a skill set or 1 skill set and a vector of marginal iq scores|1 number|descriptive models|intelligence|-|`describe`|`iq`|
|6|$\bar{\upsilon}$|skill set utility|1 skill set, 1 preference set|1 number|comparative models|utility|-|`compare`|`joy`|
|7|$u$|job satisfaction|1 skill set, 1 preference set, optionally wages, etc|1 number|comparative models|utility|-|`compare`|`joy`|
|8|$s$|skill set similarity|2 skill sets|1 number|comparative models|pairwise|similarity|`compare`|`match`|
|9|$s^{\theta} = \text{cos}{\theta}$|field similarity|2 skill sets|1 number|comparative models|pairwise|similarity|`compare`|`match`|
|10|$\delta$|gap function|2 skill sets|1 skill set|comparative models|pairwise|-|`compare`?|`match`?|
|11|uqa, $\tilde{\delta}^{<}$|underqualification|2 skill sets|1 number|comparative models|pairwise|qualification|`compare`|`qa`|
|12|oqa, $\tilde{\delta}^{\geq}$|overqualification|2 skill sets|1 number|comparative models|pairwise|qualification|`compare`|`qa`|
|13|sqa, $s^{\geq}$|sufficient qualification|2 skill sets|1 number|comparative models|pairwise|qualification|`compare`|`qa`|
|14|$\textit{ß}, \ddot{s}$|sufficient / equivalent similarity / interchangeability|2 skill sets|1 number|comparative models|pairwise|similarity|`compare`|`match`|
|15|eeq, $\ddot{\tau}$|education and experience equivalence|2 numbers, optionally competence, wage, etc|1 number|comparative models|pairwise|qualification|`compare`|`eee? edu? exp? qa? labor?`|
|16|$\tilde{T}$|productivity|2 skill sets|1 number|comparative models|pairwise|qualification|`compare`|`labor`|
|17|$h$|hireability|several statistics|1 number|comparative models|pairwise|qualification|`compare`|`labor`|
|18|ta|time allocation function|1 number|1 number|descriptive models|labor economic|-|`describe`|`ta`|
|19|$\tilde{W}$|employability|1 skill set, 1 skill set matrix, time allocation functions, etc|1 number|comparative models|pairwise & aggregate|labor|`compare`|`labor`|
|20|$\tilde{\text{vs}}$|labor market competitiveness|1 skill set, 1 skill set matrix, time allocation functions, etc|1 number|comparative models|aggregate & pairwise|labor|`compare`|`labor`|
|21|$\Lambda$|labor market strata|1 skill set matrix|1 economic taxonomy|comparative models|taxonomic|labor|`compare`|`taxa`|
|22|?|evolutionary model of occupations|1 skill set matrix|1 economic hierarchy|comparative models|taxonomic|evolutionary|`compare`|`taxa? evol?`|
|23|?|dynamic labor markets|1 skill set matrix, 1 vector of employment levels, etc|1 vector of employment levels, 1 vector of wages, etc (e.g. utility statistics)|comparative models|?|labor|`compare`|`labor`|
|24|$\hat{y}_i$|marginal compensation / labor market prices|1 skill set matrix, 1 vector of wages, 1 vector of employment levels|1 vector of marginal prices (per attribute)|microeconomic / econometric models|marginal|-|`micro`|`wage`|
|25|$\hat{\eta}$|marginal time investment|1 skill set matrix, 1 vector of years of education, experience, etc|1 vector of years of education and experience (per attribute)|microeconomic / econometric models|marginal|-|`micro`|`eta`|
|26|$\hat{\phi}$?, $\phi$|human capital microflexibility|1 skill set matrix, 1 vector of employment levels|1 microflexibility matrix|microeconomic / econometric models|marginal|microflexibility|`micro`?|`phi`?|
|27|?|leverage|1 skill set|1 marginal skill set, 1 microflexibility matrix, etc|descriptive models|human capital|microflexibility|`describe`|`kflex`?|
|29|$\hat{\Phi}$?, $\Phi$|human capital macroflexibility|1 skill set matrix, 1 vector of employment levels|1 macroflexibility skill set|microeconomic / econometric models|aggregate|macroflexibility|`micro`?|`Phi`?|
|28|?|versatility|1 skill set, 1 macroflexibility skill set|1 number|descriptive models|human capital|macroflexibility|`describe`|`kflex`?|
|30|$\hat{y}$?|skill set value|1 skill set, 1 marginal prices vector||microeconomic / econometric models|aggregate|-|`micro`?|`wage`?|
|31|$H$|total time investment|comparative models|?|?|`compare`?|`eta`?|
|32|?|letter-shaped skill sets|comparative / descriptive models|misc|letters|`describe`|`letters`|
|33|?|macro-strategy|roadmap models|career recommendation|-|`roadmap`|`goal`|
|34|$R$|preference-adjusted strategic matching|roadmap models|career recommendation|-|`roadmap`|`goal`|
|35|?|micro-strategy|roadmap models|training recommendation|-|`roadmap`|`path`|
|36|$r$|preference-adjusted strategic training|roadmap models|training recommendation|-|`roadmap`|`path`|
|37|$\Psi, \psi$|factor model|factor-analytic models|exploratory factor analysis|automation|`psi`|`efa`|
|38|?|psychometric questionnaire optimization|factor-analytic models|exploratory factor analysis|dimensionality reduction|`psi`|`efa`|
|39|?|factor-analytic comparative statics|labor economic / factor-analytic models|comparative statics|-|`psi`?|`fstatics`?|
|40|?|atlas career type indicator (ACTI)|descriptive / factor-analytic models|career type|-|`describe`|`acti`|

## Construct Dependencies