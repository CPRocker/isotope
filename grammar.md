$$
\begin{align}
[\text{Program}] &\to [\text{Statement}]^* \\
[\text{Statement}] &\to \begin{cases}
    \text{return} [\text{Expression}]; \\
    \text{let} [\text{Identifier}] = [\text{Expression}]; \\
\end{cases} \\
[\text{Expression}] &\to \begin{cases}
    [\text{BinaryExpression}] \\
    [\text{PrimaryExpression}] \\
\end{cases} \\
[\text{BinaryExpression}] &\to \begin{cases}
    [\text{MultiplicativeBinaryExpression}] \\
    [\text{AdditiveBinaryExpression}] \\
\end{cases} \\
[\text{MulitplicativeBinaryExpression}] &\to [\text{PrimaryExpression}] op [\text{PrimaryExpression}] \\
[\text{AdditiveBinaryExpression}] &\to [\text{PrimaryExpression}] op [\text{PrimaryExpression}] \\
[\text{PrimaryExpression}] &\to \begin{cases}
    ([\text{Expression}]) \\
    [\text{Identifier}] \\
    [\text{Literal}] \\
\end{cases} \\
[\text{Literal}] &\to \begin{cases}
    [\text{IntLiteral}]
\end{cases} \\
\end{align}
$$
