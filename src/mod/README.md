# Atlas Career Guide Constructs and Modules
This is a brief description of statistical methods and models in the `src/mod` directory.

## Summary Table
|id|symbol|construct|input|output|class|category|subcategory|module|submodule|
|--|------|---------|-----|------|-----|--------|-----------|------|---------|
|1|$\tilde{a}$|maxima-normalized skill set|1 skill set|1 skill set|descriptive models|basic|attribute|-|-|
|2|$\gamma$|skill set generality|1 skill set|1 number|descriptive models|generality|-|`describe`|`gene`|
|3|$\ddot{a}$|attribute equivalence|1 skill set|1 skill set|descriptive models|equivalence|attribute|`describe`|`eqvl`|
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
|30|$\hat{y}$?|skill set value|1 skill set, 1 marginal prices vector|1 number|microeconomic / econometric models|aggregate|-|`micro`?|`wage`?|
|31|$H$|total time investment|2 skill sets, 1 marginal time investment vector, 1 microflexibility matrix, etc|1 number|comparative models|?|?|`compare`?|`eta`?|
|32|?|letter-shaped skill sets|1 skill set, 1 letter-shaped skill set|1 number|comparative / descriptive models|misc|letters|`describe`|`letters`|
|33|?|macro-strategy|1 vector of user macro-priorities|1 vector of macro-strategies|roadmap models|career recommendation|-|`roadmap`|`goal`|
|34|$R$|preference-adjusted strategic matching|2 skill sets, 1 vector of macro-strategies, etc|1 number|roadmap models|career recommendation|-|`roadmap`|`goal`|
|35|?|micro-strategy|1 vector of user micro-priorities|1 vector of micro-strategies|roadmap models|training recommendation|-|`roadmap`|`path`|
|36|$r$|preference-adjusted strategic training|2 skill sets, 1 vector of micro-strategies, etc|1 training recommendation skill set vector|roadmap models|training recommendation|-|`roadmap`|`path`|
|37|$\Psi, \psi$|factor model|1 skill set matrix, 1 vector of employment levels|$F$ factors and a ss factor loadings matrix|factor-analytic models|exploratory factor analysis|automation|`psi`|`efa`|
|38|?|psychometric questionnaire optimization|1 factor model, 1 skill set matrix, 1 vector of employment levels, etc|reduced questionnaires|factor-analytic models|exploratory factor analysis|dimensionality reduction|`psi`|`efa`|
|39|?|factor-analytic comparative statics|1 ss factor loadings matrix, 1 skill set, 1 vector of employment levels, 1 vector of exogenous impacts|1 impact-adjusted skill set, summary statistics|labor economic / factor-analytic models|comparative statics|-|`psi`?|`fstatics`?|
|40|?|atlas career type indicator (ACTI)|1 skill set, 1 factor model|1 career type|descriptive / factor-analytic models|career type|-|`describe`|`acti`|
|41|-|automated plotting|1 data frame, etc|ggplots|plotting|-|-|`plot`|-|

## Dependencies
### Construct Dependencies
Modules are to be grouped in accordance with functionality and their requirements. Therefore, for instance, all constructs that only require a single skill set to be estimated ought to be placed in the same module, within submodules, if adequate. This subsection maps out which constructs depend on which, so that they can be properly modularized.

|id|symbol|construct|dependencies|requires multiple attribute vectors|requires given data|requires estimated data|
|--|------|---------|------------|-----------------------------------|-------------------|-----------------------|
|1|$\tilde{a}_k$|maxima-normalized skill set|$a_k$|0|0|0|
|2|$\gamma_k$|skill set generality|$\tilde{a}_k$|0|0|0|
|3|$\ddot{a}_k$|attribute equivalence|$\tilde{a}_k, \gamma_k$|0|0|0|
|4|$c_k$|skill set competence|$a_k, \ddot{a}_k$|0|0|0|
|5|$\hat{g}_k$|estimated intelligence|$a_k, \Psi, \psi$|0|0|1|
|6|$\bar{\upsilon}_k$|skill set utility|$a_q, \upsilon_k$|1|0|0|
|7|$u_k$|job satisfaction|$a_q, \upsilon_k, y_k,\dots$|1|1|0|
|8|$s_{kq}$|skill set similarity|$a_k, a_q, \ddot{a}_k, \ddot{a}_q$|1|0|0|
|9|$s^{\theta} = \text{cos}{\theta}$|field similarity|$a_k, a_q, \ddot{a}_k, \ddot{a}_q$|1|0|0|
|10|$\delta_{kq}$|gap function|$a_k, a_q$|1|0|0|
|11|$\tilde{\delta}_{kq}^{<}$|underqualification|$a_k, a_q$|1|0|0|
|12|$\tilde{\delta}_{kq}^{\geq}$|overqualification|$a_k, a_q$|1|0|0|
|13|$s_{kq}^{\geq}$|sufficient qualification|$a_k, a_q$|1|0|0|
|14|$\textit{\ss}_{kq}, \ddot{s}_{kq}$|sufficient / equivalent similarity / interchangeability|$a_k, a_q, \ddot{a}_k, \ddot{a}_q, \dots$|1|0|0|
|15|$\ddot{\tau}_{kq}$|education and experience equivalence|$\tau_k, \tau_q, \dots$|0|1|0|
|16|$\tilde{T}_{kq}$|productivity|$a_k, a_q, \ddot{a}_k, \ddot{a}_q$|1|0|0|
|17|$h_{kq}$|hireability|$\dots$|1|1|0.5|
|18|$\text{ta}_k$|time allocation function|-|0|0|0|
|19|$\tilde{W}_k$|employability|$a_k, \text{A}, \text{ta}_q, \dots$|1|1|0.5|
|20|$\tilde{\text{vs}}$|labor market competitiveness|$a_k, \text{A}, \text{ta}_q, u_{qk}, \dots$|1|1|0.5|
|21|$\Lambda$|labor market strata|$\text{S}_{\Theta}, \dots$|1|0|0|
|22|?|evolutionary model of occupations|$\text{A}, \dots$|1|0|0|
|23|?|dynamic labor markets|$\text{A}, w, y, u, \dots$|1|1|0.5|
|24|$\hat{y}_i$|marginal compensation / labor market prices|$\text{A}, w, y$|1|1|0|
|25|$\hat{\eta}$|marginal time investment|$\text{A}, w, \tau, \phi?, \dots$|1|1|0.5|
|26|$\hat{\phi}$?, $\phi$|human capital microflexibility|$\text{A}, w, \dots?$|1|1|0|
|27|?|leverage|$a_k, \phi$|0|0|1|
|28|$\hat{\Phi}$?, $\Phi$|human capital macroflexibility|$\text{A}, w$|1|1|0|
|29|?|versatility|$a_k, \Phi$|0|0|1|
|30|$\hat{y}$?|skill set value|$a_k, \hat{y}_i$|0|0|1|
|31|$H$|total time investment|$a_k, a_q, \eta, \phi?, \dots$|1|0|1|
|32|?|letter-shaped skill sets|$a_k, \dots$|1|1|0.5|
|33|?|macro-strategy|-|0|1|0|
|34|$R$|preference-adjusted strategic matching|$a_k, a_q, \ddot{a}_k, \ddot{a}_q, y, w, \tilde{W}, \tilde{\text{vs}}, \Phi, \phi, \dots$|1|1|1|
|35|?|micro-strategy|-|0|1|0|
|36|$r$|preference-adjusted strategic training|$a_k, a_q, \ddot{a}_k, \ddot{a}_q, y, w, \tilde{W}, \tilde{\text{vs}}, \Phi, \phi, \dots$|1|1|1|
|37|$\Psi, \psi$|factor model|$\text{A}, w$|1|1|0|
|38|?|psychometric questionnaire optimization|$\Psi, \psi, \text{A}, w, \dots$|1|1|1|
|39|?|factor-analytic comparative statics|$\Psi, \psi, \text{A}, w, \dots$|1|1|1|
|40|?|atlas career type indicator (ACTI)|$a_k, \Psi, \psi$|0|0|1|

### Modular Dependencies