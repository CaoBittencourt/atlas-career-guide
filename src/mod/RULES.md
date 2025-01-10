1. proper vectorization with composition from basic functions
2. multiple dispatch pattern with private helper functions dispatched via an export main function
3. because apply (lapply, map, sapply, etc) functions map on data frame columns, the skill_mtx must be of the form
    |occupation 1| ... |occupation n|
    |------------|-----|------------|
    |attribute 11| ... |attribute 1n|
    |attribute 21| ... |attribute 2n|
    |    ...     | ... |    ...     |
    |attribute m1| ... |attribute mn|
even though attributes are variables and, should, therefore, be the columns. Pragmatically, then, the skill set matrix is assumed to be pivoted throughout this source code (even if not so in the papers)
4. use matrix and vector operations whenever appropriate (even preferable over apply function)
?. skill_set |> assert$as.skill_mtx() |> ls?apply(function) |> return()