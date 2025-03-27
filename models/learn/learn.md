# `learn`: learning model

# The Learning Model
## Equivalent Education and Experience
Let $\tau_q \geq 0$ be the number of years of education in a related field required by occupation $q$ and $x_q \geq 0 $ the requirement for years of experience in a related field. Furthermore, we define $s_{kq}^{\theta} \in [0,1] \ \forall \ k,q \in \{1, ..., n\}$, that is *field similarity*, to be the criterion that determines what "counts" as related work experience or education. Finally, let $?_t \in \{0,1\}$ denote the nature of the time spent on an activity at time $t \geq 0$ (whether educational or work-related), so that years of education are differentiated from work experience. With this,

```math
\begin{gather}
\tau_{kq}^{z} := \sum_{t=1}^{z} [?_t = 1] s_{kq}^{\theta} \geq 0
\end{gather}
```
and 
```math
\begin{gather}
x_{kq}^{z} := \sum_{t=1}^{z} [?_t = 0] s_{kq}^{\theta} \geq 0
\end{gather}
```

represent the aggregate amount of time spent studying for $q$ and working at a $q$-related occupation, respectively, at time $z > 0$ (from the german *zeit*).

At last, one might transform these variables in accordance to scaling functions and arrive at what we term *equivalent* education and experience. Here, we use the *umlaut* equivalence operator to denote such functions:

```math
\begin{gather}
\tau_{kq}^{z} \geq \ddot{\tau}_{kq}^{z} \geq 0 ; 
\\
x_{kq}^{z} \geq \ddot{x}_{kq}^{z} \geq 0.
\end{gather}
```

Now, because most occupations have educational and work experience requirements, $\ddot{\tau}_{kq}^{z}$ and $\ddot{x}_{kq}^{z}$ shall be instrumental in determining valid career paths, as any value below a certain threshold means an occupation is not (yet) attainable.

## Skill Set Morphing
To simplify the process of learning an occupation's skill set – in a still realistic manner –, we employ the concept of "morphing". Intuitively, it means one's skill set changes the more they spend their time on a certain activity. In this sense, then, an individual's skills "morph" into those of their occupation the more they are exposed to its tasks.

Mathematically, it can be implemented via linear interpolation or similar procedures. Thus, for instance,

```math
\begin{gather}
a_{iz}^{kq} := 

a_{i1}^{k} + 
\sum_{t=1}^{z}
(
    a_{i}^{q}
    -
    a_{i}^{k}
) \times (t / z)
\in
[0,1]
\
\forall
\
i \in \{1, \dots, m \}
,
\end{gather}
```

is the simplest method to model individual $k$'s "morphing" into occupation $q$'s skill set, yielding an

```math
\begin{gather}
\boldsymbol{a_{kq}^{z}}
=
\boldsymbol{a_{q}}
\in
[0,1] ^ m
\end{gather}
```

vector of "morphed" attributes at the end of the process.

Again, this procedure is merely the simplest model available, which can be improved with time-scaling functions, learning curve functions, skill depreciation and/or conservation rates, such that the morphed skill set

```math
\begin{gather}
\boldsymbol{a_{kq}^{z}}
\neq
\boldsymbol{a_{q}}
\in
[0,1] ^ m
\end{gather}
```

and the trajectory towards it might be slower and/or faster than would be with regular linear interpolation.

## Education, Experience and Career Path Validation
As pointed out above, a career path is only valid if certain thresholds are reached. Thus, for example, if $h_{kq}^{t} \in [0,1]$ is the hireability of person $k$ at occupation $q$'s job posts at time $t$, then

```math
\begin{gather}
\ddot{\tau}_{kq}^{t} < \tau_q \implies h_{kq}^{t} = 0
,
\\
\ddot{x}_{kq}^{t} < x_q \implies h_{kq}^{t} = 0
,
\end{gather}
```

meaning one must at least satisfy the minimum educational and experience requirements to be hireable.

This is, of course, reasonable. But, so far, we have not introduced in the model any manner for individuals to meet such requirements. This means that, if they didn't start with $\ddot{\tau}_{kq}^{1} \geq \tau_q$ they might never reach it (and, likewise, for work experience), so that they'd never be hireable from the very beginning.

Luckly, given our conceptual framework, we have quite a good solution for this, which is to understand education itself as a kind of "occupation" with 
(much) lower requirements (or none at all). Therefore, let

```math
\boldsymbol{a_{q}^{\tau}} = \boldsymbol{a_{q}} \in [0,1] ^ m
```

be the vector of a course which prepares its students to be competent at an occupation $q$ and any jobs similar to it. If it is a university course, its educational requirements shall be simply, say,

```math
\begin{gather}
\tau_{kq{\tau}}^{z} := \sum_{t=1}^{z} [?_t = 1] \times 1
,\\
s_{kq{\tau}}^{\theta} = 1 \ \forall \ k,{q\tau} \in \{1, \dots, n\}
,\\
\tau_{q{\tau}} := 18
\implies
h_{kq{\tau}}^{z} = 1
\iff
\tau_{kq{\tau}}^{z}
\geq 18
,
\end{gather}
```

that is, having 18 years of education (i.e. having gone through the entire school system and graduated from high-school); or, if it were a graduate program, with a Bachelor's Degree as a prerequisite, they could be

```math
\begin{gather}
\tau_{kq{\tau}}^{z} := 
\sum_{t=1}^{18} [?_t = 1] \times 1
+
\sum_{t=18}^{21} [?_t = 1] s_{kq{\tau}}^{\theta}
,\\
s_{kq{\tau}}^{\theta} \in [0,1] \ \forall \ k,{q\tau} \in \{1, \dots, n\}
,\\
\tau_{q{\tau}} := 21
\implies
h_{kq{\tau}}^{z} = 1
\iff
\tau_{kq{\tau}}^{z}
\geq 21
,
\end{gather}
```

assuming the model starts from $t=1$ and we're at $t=21$ (if this is not the case, the formula would be slightly more complicated, but the idea remains the same).

Finally, let us demonstrate how framing education as "a kind of occupation" resolves the problems we had before. If person $k$ wants to work as a $q$, but does not yet meet its educational requirements,

```math
\begin{gather}
\because
\tau_{kq}^{1} < \tau_{q},
\end{gather}
```

they might enroll in a university program $q\tau$ for which they are eligible,

```math
\begin{gather}
\because 
\tau_{k{q\tau}}^{1} \geq \tau_{q\tau}
,
\end{gather}
```

and, by the end of it, be fully prepared both in terms of education and skill set similarity to be hired at occupation $q$'s job posts,

```math
\begin{gather}
\because
\boldsymbol{a_{q}^{\tau}}
=
\boldsymbol{a_{q}}

\implies
\tau_{k{q\tau}}^{z}
=
\sum_{t=1}^{z} [?_t = 1] s_{{q\tau}q}^{\theta}
=
\sum_{t=1}^{z} 1 \times 1
=
\sum_{t=1}^{z}
=
z

\\
\land
\
a_{iz}^{k{q\tau}}
=
a_{i1}^{k} + 
\sum_{t=1}^{z}
(
    a_{i}^{q\tau}
    -
    a_{i}^{k}
) \times (t / z)
\
\forall
\
i \in \{1, \dots, m \}

\implies
\boldsymbol{a_{k{q\tau}}^{z}}
=
\boldsymbol{a_{q}^{\tau}}

\\
\therefore
\boldsymbol{a_{k{q\tau}}^{z}}
=
\boldsymbol{a_{q}^{\tau}}
=
\boldsymbol{a_{q}}

\iff
s_{k{q\tau}}^{z}
=
s_{{q\tau}q}
=
s_{qq}
=
1

\\
\therefore
\tau_{k{q\tau}}^{z\theta}
=
z
\geq
\tau_q
\
\forall
\
z \geq \tau_q 
\land
\
s_{k{q\tau}}^{z} = 1
\implies
h_{k{q\tau}}^{z\theta}
= 1
,
\end{gather}
```

assuming they are hireable given other selection criteria. Thus, person $k$'s skill set "morphs" into that of occupation $q$, while their university program having a perfect field similarity with $q$ guarantees all $z$ years they put into it are counted as valid for this occupation.

# The Learning Model and Career Strategy
