# `roadmap` module
Helps individuals in all steps of their career journey.

## `goal` submodule
Helps individuals choose their career, given their macro-strategy.

```math
\begin{gather}
B_{k}^{*} = 
\argmax_{
    q \ \in \ \{1, \ \dots, \ n\}
}{
    U_{kq}^{*}
},
\\
B_{k}^{t} = B_{k}^{*}
\implies
B_{k}^{z} = B_{k}^{*}
\
\forall
\
z \geq t \in \{1, \dots, \bar{\tau}\}
\end{gather}
```

## `path` submodule
Helps individuals optimize training, given their micro-strategy.

```math
\begin{gather}
\ddot{x}_{kq}^{z}
=
\sum_{t=1}^{z}{
    \left[B_{k}^{t} \in \{1, \dots, n\}\right]
    \left[s_{B_{k}^{t}q}^{\theta} \geq \frac{1}{2}\right]
    s_{B_{k}^{t}q}^{\theta}
}
,
\\
\ddot{\tau}_{kq}^{z}
=
\sum_{t=1}^{z}{
    \left[B_{k}^{t} \in \{n+1, \dots, 2n\}\right]
    \left[s_{B_{k}^{t}q}^{\theta} \geq \frac{1}{2}\right]
    s_{B_{k}^{t}q}^{\theta}
}
,
\\
B_{k}^{z}
=
B_{k}^{*}
[\ddot{\tau}_{kB_{k}^{*}}^{z} \geq \tau_{B_{k}^{*}}]
[\ddot{x}_{kB_{k}^{*}}^{z} \geq x_{B_{k}^{*}}]
+ 
\left(
    1 - 
[\ddot{\tau}_{kB_{k}^{*}}^{z} \geq \tau_{B_{k}^{*}}]
[\ddot{x}_{kB_{k}^{*}}^{z} \geq x_{B_{k}^{*}}]
\right)
\argmax_{q \ \in \ \{1, \ \dots, \ 2n\}}{
    \left(
        [\ddot{\tau}_{kq}^{z} \geq \tau_{q}]
        [\ddot{x}_{kq}^{z} \geq x_{q}]
        \left(
            [\ddot{x}_{kB_{k}^{*}}^{z} < x_{B_{k}^{*}}]
            [q \in \{1, \dots, n\}] 
            +
            [\ddot{\tau}_{kB_{k}^{*}}^{z} < \tau_{B_{k}^{*}}]
            [q \in \{n+1, \dots, 2n\}]
        \right)
        % \left[s_{qB_{k}^{*}}^{\theta} \geq \frac{1}{2}\right]
        s_{qB_{k}^{*}}^{\theta}
        U_{kq}
    \right)
}
\end{gather}
```
